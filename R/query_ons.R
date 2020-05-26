#' Run a SPARQL query on the ONS Geography Linked Data API
#'
#' The query returns a data frame with the `results` element of the list return
#' by the API query. For documentation of the API see
#' http://statistics.data.gov.uk/sparql.
#'
#' @param query Character. SPARQL query for the API.
#' @param ... Other arguments passed to SPARQL::SPARQL().
#'
#' @importFrom RCurl getURL
#' @import XML
#' @importFrom SPARQL SPARQL
#' @export

query_ons <- function(query, ...) {
  tryCatch({
    SPARQL::SPARQL(url = "http://statistics.data.gov.uk/sparql",
                   query = query, ...)$results},
    error = function(e) stop(paste("The ONS SPARQL query returned an error:", e, sep = "\n "))
  )
}
