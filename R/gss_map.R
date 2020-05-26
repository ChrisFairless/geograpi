#' Map a given set of GSS codes
#'
#' @param data A data frame containing a column with gss data, or a vector of
#'   gss codes or names. These should be provided in plotting order.
#' @param col_gss If \code{data} is a data frame, the name of the column
#'   containing gss codes or names.
#'
#' @return The input data frame or vector with an added column of sf objects
#'
#' @import dplyr
#' @import SPARQL
#' @import assertthat
#' @export

gss_map <- function(data,
                    col_gss = "gss_code",
                    #col_return = c("LSOA", "MSOA", "LAD"),
                    col_return = NULL,
                    vintage = NULL,
                    limit = NULL) {

  # Validate input data
  # ===================

  data  <- .validate_gss_input_and_reformat(data, col_gss)

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
  sparql_query[2] <- paste(
    'SELECT DISTINCT ?gss_code ?gss_name ?geometry',
    'WHERE {',
    '?gss_uri rdfs:label ?gss_code ;',
    '         geog-def:officialname ?gss_name ;',
    '         opengis:hasGeometry ?gss_geom_uri .',
    '?gss_geom_uri opengis:asWKT ?geometry',
    sep="\n")

  # 3: Filter by the GSS codes we care about
  sparql_query[3] <- paste0(c('FILTER( ?gss_code IN ( ', gss_string, '))'), collapse='')

  sparql_query[4] <- "} "

  # Stick all the above together and make the query
  full_query <- paste(sparql_query, collapse="\n")
  response <- query_ons(full_query)


  # Wrangle the results
  # ===================

  # Check all input gss codes were matched
  ix <- gss_codes %in% response$gss_code
  if(!all(ix)) {
    warning(paste(c("Not all GSS codes were found in the ONS database. \nMissing:",
                    gss_codes[!ix], collapse = " ")))
  }

  # The geometry data comes through slightly odd- not sure if I'm missing something
  get_geometry <- function(s) {
    strsplit(s, "\"")[[1]][2] %>%
      rgeos::readWKT()
  }
  geometries <- lapply(response$geometry, get_geometry)



  # Plot the results
  # ===================



  if(nrow(response) == 0) {
    warning("No results found in gss_lookup, returning NULL")
    return(NULL)
  }

  leaflet(geometries)




  invisible(response)
}

