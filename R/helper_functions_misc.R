
# HELPER FUNCTIONS

#===============================================================================

# Convert a URI string denoting a date to a date string or a date object
# Input should be of the format "<http://reference.data.gov.uk/id/day/2011-12-31>"
# The parameter 'format' determines whether a string or date object is returned
# Vectorisable

.date_from_uri <- function(uri, format = "date") {

  assert_that(format %in% c("string", "date"))
  assert_that(all(is.na(uri) | is.character(uri)))

  uri <- ifelse(uri == "<NA>", NA, uri)

  ix <- !is.na(uri) &&  nchar(uri) < 11
  if(any(ix)) {
    warning(paste(c(".date_from_uri() was given an unexpected string.",
                    "This will set to NA:", uri[ix], collapse = " ")))
    uri[ix] <- NA
  }

  iy <- !is.na(uri)
  uri[iy] = substr(uri[iy], nchar(uri[iy]) - 10, nchar(uri[iy]) - 1)

  iz <- !grepl("\\d{4}-\\d{2}-\\d{2}", uri[iy])
  if(any(iz)) {
    warning(paste(c(".date_from_uri was given an unexpected string.","
                    This will be set to NA:", uri[iz]), collapse = " "))
    uri[iz] <- NA
  }

  if(format == "string") return(uri)
  if(format == "date") return(as.Date(uri))
}


#===============================================================================

# Return TRUE or FALSE if input can be coercible to an integer. Vectorisable.
.coercible_to_integer <- function(x) {
  x <- suppressWarnings(as.integer(x))
  return(!is.na(x))
}
