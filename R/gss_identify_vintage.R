#' Identify the vintage of a set of GSS codes
#'
#' Given a data frame or a vector containing GSS codes, return a list with
#' values of the earliest and latest dates all input codes were valid, plus
#' other data.
#'
#' @param data A vector of GSS codes or a data frame.
#' @param col_gss If \code{data} is a data frame, the name of the column
#'   containing gss_codes.
#' @param test_vintage Either NULL, a year, or a date giving the hypothesised
#'   vintage of the data. The returned list contains elements confirming whether
#'   this vintage is compatible with the input data. When this is provided as a
#'   year, it is assumed to be December 31 of that year, the date usually
#'   referred to by single-year vintages.
#'
#' @return A list with several elements: \code{vintage_start} is the earliest
#'   date compatible with the input data, and \code{vintage_end} is the latest.
#'   \code{vintage_years} is a vector of years where the 31 December is within
#'   the interval between \code{vintage_start} and \code{vintage_end}.
#'   \code{vintage_current} is a boolean indicating whether the data is current.
#'   \code{test_vintage} is the date given by the input parameter
#'   \code{test_vintage} and \code{test_valid} is a boolean denoting whether the
#'   vintage is consistent with the input data. When no vintage is consistent
#'   with these data these parameters take \code{NA}.
#'
#' @examples
#' \dontrun{
#'   # Find the vintage of the City of London
#'   gss_identify_vintage("E09000001")
#'
#'   # Check the wards in a data frame are consistent
#'   x <- data.frame(name = c("a", "b"),
#'                   gss_code = c("E05000610", "E05000615"))
#'   gss_identify_vintage(x, col_gss = "gss_code", test_vintage = 2011)
#' }
#' @import dplyr
#' @export

gss_identify_vintage <- function(data, col_gss = "gss_code", test_vintage = NULL) {

  # TODO make this work with multiple gss columns
  data <- .validate_gss_input_and_reformat(data, col_gss)
  test_vintage <- .validate_vintage_and_reformat(test_vintage)

  if(is.data.frame(data)) {
    gss_codes <- data[[col_gss]]
  } else {
    gss_codes <- data
  }

  timeline <- gss_timeline(gss_codes = gss_codes, plot = NULL)

  # TODO Split the following into an internal function to allow for testing
  min_vintage <- max(timeline$created)
  max_vintage <- min(timeline$terminated)
  active <- all(timeline$active)

  if(min_vintage > max_vintage) {
    # No valid vintage for this period
    out <- list(vintage_start = NA,
                vintage_end = NA,
                vintage_years = c(),
                vintage_current = FALSE,
                test_vintage = ifelse(is.null(test_vintage), NA, test_vintage),
                test_valid = ifelse(is.null(test_vintage), NA, FALSE))
    return(out)
  }

  # Vintage years are all instances of 31 December contained in the returned interval
  vintage_year_start <- format(min_vintage, "%Y") %>% as.numeric()
  vintage_year_end <- format(max_vintage, "%Y") %>% as.numeric()
  if(format(max_vintage, "%m-%d") != "12-31") {
    vintage_year_end = vintage_year_end - 1
  }

  vintage_years <- vintage_year_start:vintage_year_end

  if(is.null(test_vintage)) {
    test_vintage <- NA
    test_valid <- NA
  } else {
    test_valid <- (test_vintage >= min_vintage) && (test_vintage <= max_vintage)
  }

  return(list(vintage_start = min_vintage,
              vintage_end = max_vintage,
              vintage_years = vintage_years,
              vintage_current = active,
              test_vintage = test_vintage,
              test_valid = test_valid))
}
