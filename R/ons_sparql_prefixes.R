#' Shortcuts to tables used in the ONS SPARQL queries
#' @export
ons_sparql_prefixes <- function() {
  paste(
    'PREFIX geog-def:<http://statistics.data.gov.uk/def/statistical-geography#>',
    'PREFIX within:<http://publishmydata.com/def/ontology/foi/>',
    'PREFIX gss_uri:<http://statistics.data.gov.uk/def/statistical-entity#>',
    'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>',
    'PREFIX change: <http://statistics.data.gov.uk/def/boundary-change/>',
    'PREFIX opengis: <http://www.opengis.net/ont/geosparql#>',
  sep = "\n")
}
