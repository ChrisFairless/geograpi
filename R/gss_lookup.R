#' Get data on GSS hierarchies
#'
#' Given a data frame or a vector containing standardised names or codes of UK
#' statistical geographies, return a data frame with a column for each geography
#' that the input geographies are contained within.
#'
#' Currently only works for official parent geographies, so e.g. 2011 LSOAs are
#' not contained within 2011 wards.
#'
#' Note that calls to the ONS Open Geography API limit response size to 10 MB.
#' If you're looking for the GSS hierarchy for the whole of the UK, you can use
#' the function with the same name in the \code{geogr.uk} package where data are
#' downloaded with the package and stored locally, or get it manually from
#' <TODO: URL>
#'
#' @param data A data frame containing a column with gss data, or a vector of
#'   gss codes or names.
#' @param col_gss If \code{data} is a data frame, the name of the column
#'   containing gss codes or names.
#' @param col_return Names of resolutions to return. Can be names (e.g. LSOA),
#'   gss prefixes (e.g. E01), and optionally can have 2-digit vintage year
#'   and/or 'NM' or 'CD' suffixes, e.g. LSOA, LSOA11, LSOA11NM, E01, E0111,
#'   E0111CD. If no CD/NM suffix is specified, both will be returned, The
#'   vintage must be consistent with \code{vintage} (so is entirely cosmetic).
#'   Note that all returned column names will be in the format e.g. 'LSOA11CD'
#'   regardless of how they were specified.
#' @param vintage A date that the returned GSS codes should be valid for. The
#'   default, NULL, is interpreted as the most recent data available. A year
#'   (string or integer) gives the 31st December of that year, which is the
#'   usual way of dating ONS geographic vintages. A month or date, formatted
#'   "%Y-%m-%d" or "%Y/%m/%d" will give a date (at the end of the month if day
#'   is excluded). Also accepts Date objects. Returned column names will include
#'   the year of the vintage when the vintage provided was for December 31st, or
#'   a date was not specified within the year (e.g. LSOA11CD), otherwise it will
#'   be undated (e.g. LSOACD), unless \code{col_return} specifies the names.
#' @param limit Integer. The maximum number of rows to return. NULL is
#'   unlimited.
#'
#'
#' @import dplyr
#' @import SPARQL
#' @import assertthat
#' @export

gss_lookup <- function(data,
                       col_gss = "gss_code",
                       #col_return = c("LSOA", "MSOA", "LAD"),
                       col_return = NULL,
                       vintage = NULL,
                       limit = NULL) {

  # TODO decide what the default vintage should be - 2011 or present day
  # TODO add option to insist all input is the same resolution? (might cause scilly troubles)
  # TODO extend the query to include best-fit hierarchies? e.g. 2011 lsoa-ward-lad
  # TODO allow more than one vintage to be supplied
  # TODO test how big the response can be - does it fall over at OA/LSOA/... and add to documentation
  # TODO deal with column name conflicts
  # TODO deal with non-existent resolution codes
  # TODO deal with non-existend gss codes

  # Validate input data
  # ===================

  data       <- .validate_gss_input_and_reformat(data, col_gss)
  col_return <- .validate_col_return_and_reformat(col_return, vintage)
  vintage    <- .validate_vintage_and_reformat(vintage)
  limit      <- .validate_limit(limit)

  # TODO keep this outside the validate call for now
  if(!is.data.frame(data)) {
    gss_codes <- data
  } else {
    gss_codes <- unique(data[[col_gss]])
  }

  gss_string <- paste0('"', paste0(gss_codes, collapse='", "'), '"')

  # Construct a SPARQL query
  # ========================
  sparql_query <- c()

  # 1: SPARQL prefixes
  sparql_query[1] <- ons_sparql_prefixes()

  # 2: Main query
  if(is.null(vintage)) {
    # If we want current geographies, filter by active = true
    sparql_query[2] <- paste(
      'SELECT DISTINCT ?gss_code ?gss_name ?parent_code ?parent_name ?resolution_abbreviation',
      'WHERE {',
        '?gss_uri within:within ?gss_parent;',
                 'within:active true;',
                 'geog-def:officialname ?gss_name;',
                 'rdfs:label ?gss_code .',
        '?gss_parent rdfs:label ?parent_code ;',
                    'geog-def:officialname ?parent_name ;',
                    'gss_uri:code ?parent_resolution .',
        '?parent_resolution gss_uri:abbreviation ?resolution_abbreviation',
      sep="\n")

  } else {
    # Otherwise get the creation and termination dates of all geographies (we'll filter later)
    sparql_query[2] <- paste(
      'SELECT DISTINCT ?gss_code ?gss_name ?parent_code ?parent_name ?resolution_abbreviation
                       ?parent_startdate ?parent_enddate',
      'WHERE {',
        '?gss_uri within:within ?gss_parent ;',
                 'geog-def:officialname ?gss_name ;',
                 'rdfs:label ?gss_code .',
        '?gss_parent rdfs:label ?parent_code ;',
                    'geog-def:officialname ?parent_name ;',
                    'gss_uri:code ?parent_resolution ;',
                    'change:operativedate ?parent_startdate .',
        '?parent_resolution gss_uri:abbreviation ?resolution_abbreviation',
        'OPTIONAL { ?gss_parent change:terminateddate ?parent_enddate }',
      sep="\n")
  }

  # 3: Filter by the GSS codes we care about
  if(!is.null(gss_codes)) {
    sparql_query[3] <- paste0(c('FILTER( ?gss_code IN ( ', gss_string, '))'), collapse='')
  } else {
    sparql_query[3] <- ''
  }

  sparql_query[4] <- "} "

  # 4: Limit the number of results returned
  if(!is.null(limit)) {
    sparql_query[5] <- paste("LIMIT", limit)
  } else {
    sparql_query[5] <- ''
  }

  # Stick all the above together and make the query
  full_query <- paste(sparql_query, collapse="\n")
  response <- query_ons(full_query)



  # Wrangle the results
  # ===================

  # Check all input gss codes were matched (before date filtering)
  ix <- gss_codes %in% response$gss_code
  if(!all(ix)) {
    warning(paste(c("Not all GSS codes were found in the ONS database. \nMissing:",
                    gss_codes[!ix], collapse = " ")))
  }

  # Filter to the right dates (if we didn't want the most recent data)
  if(!is.null(vintage)) {
    # Dates are returned in the format "<http://reference.data.gov.uk/id/day/2004-08-01>"
    response$parent_startdate <- .date_from_uri(response$parent_startdate, format = "date")

    iy <- !is.na(response$parent_enddate)
    if(any(iy)) {
      response$parent_enddate[iy] <- .date_from_uri(response$parent_enddate[iy], format = "string")
    }
    response$parent_enddate[!iy] <- Sys.Date() %>% format("%Y-%m-%d")

    response$parent_enddate <- as.Date(response$parent_enddate)

    # vintage <- as.Date(vintage)

    # TODO maybe dtplyr would be helpful here
    response <- response %>%
      filter(vintage >= parent_startdate,
             vintage <= parent_enddate)

    iz <- gss_codes %in% response$gss_code
    ix_excluded <- (ix & !iz)
    if(any(ix_excluded)) {
      warning(paste(c("Not all GSS codes were valid for vintage", format(vintage, "%Y-%m-%d"),
                      "\nExcluded:", gss_codes[ix_excluded]), collapse = " "))
    }

  } else {
    # TODO how do we label a vintage that's not the december of a year?
    vintage <- Sys.Date()
  }
  vintage_string <- format(vintage, "%y")

  if(nrow(response) == 0) {
    warning("No results found in gss_lookup, returning NULL")
    return(NULL)
  }

  # Then, a whole load of pivoting to make a new column for each resolution in the returned hierarchy
  # and to give them familiar names
  response <- rename(response, CD=parent_code, NM=parent_name) %>%
    tidyr::pivot_longer(cols = c("CD", "NM"),
                        names_to = "type", values_to = "value")

  # Column names by name, e.g. LSOA, LAD
  response <- tidyr::pivot_wider(response,
                                 names_from = c("resolution_abbreviation", "type"),
                                 values_from = "value",
                                 names_sep = vintage_string)


  if(is.data.frame(data)) {
    # TODO use dtplyr here to speed things up?
    # TODO deal with potential column name conflicts
    response <- left_join(data, response, by = {{col_gss}})
  }

  return(response)
}


