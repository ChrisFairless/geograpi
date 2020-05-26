#' Function to query and plot the history of changes to a single GSS code, or
#' GSS resolution
#'
#' The function draws a timeline of changes to the gss_code or whole resolution;
#' it doesn't describe the changes (yet).
#'
#' Used in particular for checking consistency with vintages: check for an empty
#' result.
#'
#' See also gss_reaggregate(?) for converting gss codes between vintages.
#'
#' @param resolution A character of gss resolutions, e.g. OA, LSOA.
#' @param gss_codes A character vector of gss codes to limit the results to.
#'   Overrides \code{resolution}.
#' @param startdate  A date, integer or string that can be read as a vintage.
#'   The first date of the period for which you want to extract any changes to
#'   the geography. The default, NULL, is interpreted as the earliest date
#'   available. A year (e.g. 2011) (string or integer) gives the 31st December
#'   of that year, which is the usual way of dating ONS geographic vintages. A
#'   month or date, formatted "%Y-%m-%d" or "%Y/%m/%d" will give a date (at the
#'   end of the month if day is excluded). Also accepts Date objects.
#' @param enddate  As \code{startdate} but for the end of the period you're
#'   interested in. Default is NULL, which is the most recent data available.
#' @param plot When NA no plot is drawn. When \code{"line"} a timeline is drawn
#'   grouping geographies by creation and termination date. When \code{"sankey"}
#'   a Sankey diagram is drawn. Both diagrams can be overly noisy if the
#'   selected resolutions, GSS codes or time periods contain too many elements
#'   and can be refined by returning the data via the \code{return_list}
#'   command, editing, and passing to the package's
#'   \code{plot_gss_timeline_line()} or \code{plot_gss_timeline_sankey()}
#'   functions.
#' @param sankey_scale Either "linear" or "log". Whether to scale flows in
#'   output Sankey diagrams by their values or the logarithm of their values.
#'   (Note that nodes won't be proportional to total flows).
#' @param include_changeorder Logical. If TRUE the output contains two
#'   additional columns, \code{changeorder} and \code{changeordertitle}. These
#'   give codes and titles for the legal workings that create and terminate the
#'   geographies (and therefore potentially more output rows than when the
#'   parameter is set to FALSE since several orders can come into effect on the
#'   same date). They're not included by default because the change order names
#'   are not unique in the database, and one change order may have several
#'   slightly varying titles (see the examples), splitting up changes that ought
#'   to be considered as one.
#' @param return_list Logical. By default the function returns the wrangled
#'   output from the query as an easy-to-use data frame. For debugging and
#'   customisation set this to TRUE and instead return a list with four
#'   elements: \code{query} containing the SPARQL query sent to the ONS servers.
#'   Re-run the query by passing it to \code{query_ons()}, or print and inspect
#'   it with \code{cat()}. The query's response is stored as \code{response}.
#'   The wrangled output is stored as \code{return} - this is what the function
#'   returns when \code{return_list} is FALSE. The visualisation produced when
#'   \code{plot} is not NULL is stored as \code{plot}. If the \code{plot}
#'   parameter is \code{"sankey"} then \code{sankey_data} is returned as a list
#'   with a \code{nodes} and a \code{links} element used in the Sankey diagram.
#'
#'
#' @return Data frame with XXXXXX, plot XXXXXXX
#'
#' @examples
#' \dontrun{
#' # Get and plot the change history for all OAs, LSOAs and MSOAs
#' x <- gss_timeline(c("OA", "LSOA", "MSOA"))
#'
#' # Get and plot the change history for a single ward
#' x <- gss_timeline(gss_codes = "E05000021")
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import assertthat
#'
#' @export

# TODO think about splitting this into a query and a wrangle function that users can run separately

gss_timeline <- function(resolution = c("OA", "LSOA", "MSOA", "WARD", "LAD"),
                         gss_codes = NULL,
                         startdate = NULL,
                         enddate = NULL,
                         plot = "line",
                         sankey_scale = "log",
                         include_changeorder = FALSE,
                         return_list = FALSE) {

  if(!is.null(plot)) {
    assert_that(plot %in% c("sankey", "line"))
    assert_that(sankey_scale %in% c("log", "linear"))
  }
  startdate <- .validate_vintage_and_reformat(startdate)
  enddate   <- .validate_vintage_and_reformat(enddate)

  if(!is.null(gss_codes)) {
    gss_string <- paste0('"', paste0(gss_codes, collapse='", "'), '"')
    resolution <- NULL
  }

  # Work out if we're querying a whole gss resolution(s) or a specified subset.
  # For now this is a crude decision tree, but there might be a smarter way to do it
  classify_resolution <- function(s) {
    if(.coercible_to_integer(substr(s, 2, 3)) && nchar(s) == 3) {
      type <- "res_code"
    } else {
      type <- "res_name"
    }
  }

  if(!is.null(resolution)) {

    resolutions_like_gss_codes <- sapply(resolution, grepl, pattern = "\\w\\d{8}")
    if(any(resolutions_like_gss_codes)) {
      if(sum(resolutions_like_gss_codes) <= 10) {
        warning(paste(c("Some resolutions look like gss codes. Did you mean to pass them with the gss_codes parameter instead? \nResolutions:",
                        resolution[resolutions_like_gss_codes]), collapse = " "))
      } else {
        warning(paste(c("Some resolutions look like gss codes. Did you mean to pass them with the gss_codes parameter instead? \nFirst ten resolutions:",
                        resolution[resolutions_like_gss_codes[1:10]]), collapse = " "))
      }
    }

    resolution_class <- sapply(resolution, classify_resolution)
    resolution_codes <- resolution[resolution_class == "res_code"]
    resolution_names <- resolution[resolution_class == "res_name"]
    resolution_codes_string <- paste0('"', paste0(resolution_codes, collapse='", "'), '"')
    resolution_names_string <- paste0('"', paste0(resolution_names, collapse='", "'), '"')

    # TODO
    if(any(resolution_class == "res_code")) {
      stop("Function not yet designed to work with E01-format input resolutions")
    }
  }

  # Construct a SPARQL query
  # ========================

  nested_query <- c()

  # Query to grab all relevant gss codes and vintage information.
  # (Next we'll put this inside a grouping query later to summarise it)
  nested_query[1] <- paste(
    "SELECT DISTINCT ?gss_uri ?resolution_abbreviation ?created ?terminated ?changeorder ?changeordertitle",
    "WHERE {",
    "  ?gss_uri rdfs:label ?gss_code ;",
    "      gss_uri:code ?resolution ;",
    "      change:operativedate ?created ;",
    "      change:originatingChangeOrder ?changeorder ;",
		"		  change:changeOrderTitle ?changeordertitle .",
    "  ?resolution gss_uri:abbreviation ?resolution_abbreviation",
    "  OPTIONAL { ?gss_uri change:terminateddate ?terminated }",
    sep = "\n        ")

  if(!is.null(gss_codes)) {
    nested_query[2] <- paste0("FILTER( ?gss_code IN (", gss_string, ") )")
  } else {
    nested_query[2] <- paste0("FILTER( ?resolution_abbreviation IN (", resolution_names_string, "))")
  }

  nested_query[3] <- "}"
  full_nested_query <- paste(nested_query, collapse = "\n        ")


  # Query to group the results of the above and sum them into something manageable
  sparql_query <- c()

  sparql_query[1] <- ons_sparql_prefixes()

  sparql_query[2] <- paste(
      "SELECT ?resolution_abbreviation ?created ?terminated ?changeorder ?changeordertitle",
      "(COUNT(?gss_uri) as ?count)",
      "WHERE {",
      sep = "\n")

  sparql_query[3] <- full_nested_query

  sparql_query[4] <- "}"

  sparql_query[5] <- paste(
    "GROUP BY ?resolution_abbreviation ?created ?terminated ?changeorder ?changeordertitle",
    sep="\n")


  # Stick all the above together and make the query
  full_query <- paste(sparql_query, collapse="\n")

  query_response <- query_ons(full_query)
  response <- query_response

  # Wrangle the results
  # ===================

  # Check all input gss codes were matched (before date filtering)
  if(nrow(response) == 0 || sum(response$count) == 0) {
    warning("gss_timeline: no gss_codes were matched")
    return(NA)
  }

  nmatch <- sum(response$count)
  if(!is.null(gss_codes) && length(gss_codes) != nmatch) {
    warning("gss_timeline: not all GSS codes were found in the ONS database. Results may not be accurate.")
  }

  if(!is.null(resolution)) {
    ix <- resolution %in% response$resolution_abbreviation
    if(any(!ix)) {
      warning(paste(c("Not all requested resolutions found. Missing:", resolution[!ix]), collapse = " "))
    }
  }

  response$created <- .date_from_uri(response$created, format = "date")
  response$terminated <- .date_from_uri(response$terminated, format = "date")

  # Filter to the right dates (if we didn't want all time)
  # TODO don't - later instead
  # TODO set 'current' as a column
  ix <- is.na(response$terminated)
  response$active <- ifelse(ix, TRUE, FALSE)
  response$terminated[ix] <- Sys.Date()

  if(include_changeorder) {
    grouping_vars <- c("resolution_abbreviation", "created", "terminated", "active", "changeorder", "changeordertitle")
  } else {
    grouping_vars <- c("resolution_abbreviation", "created", "terminated", "active")
  }

  response <- group_by_at(response, grouping_vars) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    arrange(resolution_abbreviation, created, desc(terminated))

  response <- mutate(response,
                     startrelevant = ifelse(is.null(startdate), TRUE, created > startdate),
                     endrelevant   = ifelse(is.null(enddate), TRUE, terminated < enddate),
                     relevant = startrelevant & endrelevant) %>%
    select(-startrelevant, -endrelevant)

  if(!is.null(plot) && plot == "sankey") {
    if(requireNamespace("networkD3", quietly = TRUE)) {

      links <- response %>%
        transmute(resolution = resolution_abbreviation,
                  source_date = created,
                  target_date = terminated + 1,
                  length = target_date - source_date,
                  #order = changeorder,
                  count) %>%
        group_by(resolution, source_date, target_date, length) %>%
        summarise(count = sum(count)) %>%
        mutate(logcount = log(count * 10, base = 10)) %>%
        mutate(source_node = paste(resolution, source_date),
               target_node = paste(resolution, target_date)) %>%
        ungroup()

      nodes <- links %>%
        select(resolution, source_date, target_date) %>%
        tidyr::pivot_longer(cols = c("source_date", "target_date"),
                     names_to = "type", values_to = "date") %>%
        select(resolution, date) %>%
        unique() %>%
        mutate(name = paste(resolution, date),
               year = substr(date, 1, 4),
               id = (1:nrow(.)) - 1)

      links <- links %>%
        left_join(nodes, by = c("resolution", "source_node"="name")) %>%
        rename(source = id) %>%
        left_join(nodes, by = c("resolution", "target_node"="name")) %>%
        rename(target = id)

      value <- ifelse(sankey_scale == "log", "logcount", "count")

      outplot <- networkD3::sankeyNetwork(Links = links,
                                       Nodes = nodes,
                                       Source = "source",
                                       Target = "target",
                                       Value = value,
                                       NodeID = "name",
                                       NodeGroup = "year",
                                       LinkGroup = "resolution",
                                       fontFamily = "Arial",
                                       sinksRight = FALSE)
      print(outplot)
    } else {
      warning("Install package networkD3 to create Sankey plots")
      outplot <- NA
    }
  }


  if(!is.null(plot) && plot == "line") {

    lineplot_data <- response %>%
      arrange(resolution_abbreviation, desc(created), terminated) %>%
      mutate(terminated = terminated + 1, # so it matches the replacement 'created'
             #logcount = log(count * 10, base = 10),
             ycoord = resolution_abbreviation != lag(resolution_abbreviation),
             ycoord = 0.5 + (is.na(ycoord) | ycoord),
             ycoord = cumsum(ycoord) + 1) %>%
      group_by(resolution_abbreviation) %>%
      #mutate(ycoord = 1,
      #       ycoord = cumsum(ycoord)) %>%
      ungroup() %>%
      tidyr::pivot_longer(c("created", "terminated"), names_to = "mode", values_to = "date")

    aspect_ratio <- max(c(0.4, 0.4 + 0.1 * max(lineplot_data$ycoord)))
    aspect_ratio <- min(c(aspect_ratio, 1))

    outplot <- lineplot_data %>%
      ggplot(aes(x = date, y = ycoord, group = ycoord, colour = resolution_abbreviation))

    if(!is.null(startdate) && startdate >= min(lineplot_data$date) - 365) {
      outplot <- outplot + geom_rect(aes(xmin = min(lineplot_data$date) - 365, xmax = startdate,
                                   ymin = -Inf, ymax = Inf),
                               fill = "grey80", colour = "grey80") +
        geom_text(x = startdate, y = 1, label = startdate, colour = "darkred", angle = 90, size = 5)
    }
    if(!is.null(enddate) && enddate <= max(lineplot_data$date) + 365) {
      outplot <- outplot + geom_rect(aes(xmin = enddate, xmax = max(lineplot_data$date) + 365,
                                   ymin = -Inf, ymax = Inf),
                               fill = "gray80", colour = "grey80") +
        geom_text(x = enddate, y = 1, label = enddate, colour = "darkred", angle = 90, size = 5)
    }

    outplot <- outplot +
      geom_line(aes(group = date)) +
      geom_line(size = 2) +
      # TODO change point to arrow if current
      geom_point(size = 4) +
      geom_text(aes(x = date, y = 1, label = date), colour = "gray30", angle = 90, size = 5) +
      geom_text(data = filter(lineplot_data, mode == "created"),
                aes(x = date, y = ycoord,
                    label = paste0(formatC(count, big.mark = ","), " ", resolution_abbreviation, "s")),
                colour = "gray40", size = 4, nudge_y = 0.2, nudge_x = 365) +
      theme_minimal() +
      theme(aspect.ratio = aspect_ratio) +
      scale_x_date(name = "Date",
                   date_breaks = "1 year",
                   minor_breaks = unique(lineplot_data$date),
                   date_labels = "%Y",
                   limits = c(min(c(as.Date("2008-06-01"), lineplot_data$date)),
                              max(c(Sys.Date(), lineplot_data$date)))) +
      scale_y_continuous(name = "", breaks = NULL, limits = c(0, max(lineplot_data$ycoord) + 0.5)) +
      theme(axis.text.x = element_text(size = 10, angle = 90))

    print(outplot)
  }

  response <- filter(response, relevant) %>%
    select(-relevant)

  if(return_list) {
    response <- list(
      query = full_query,
      response = query_response,
      return = response,
      plot = outplot
    )
    if(!is.null(plot) && plot == "sankey") {
      response$sankey_data <- list(nodes = nodes, links = links)
    }
  }

  return(response)

}
