# Functions used to validate input to the geograpi family of functions



# Validate input gss values
#
# Checks input is either a character vector or a data frame containing a
# specified column. Doesn't check if they're real gss codes or convert format, yet.

.validate_gss_input_and_reformat <- function(data, col_gss = NULL) {

  assert_that(is.character(data) | is.data.frame(data),
              msg = "the data parameteter must be a character vector or a data frame")

  if(is.data.frame(data)) {
    assert_that(is.character(col_gss) & length(col_gss) == 1,
                msg = "the col_gss parameter must be a string")
    assert_that(col_gss %in% names(data),
                msg = paste(c("couldn't find column", col_gss,
                              "in the input data frame. Input columns:", names(data)),
                            collapse = " "))
  }

  return(data)
}


#===============================================================================


# Simple check on the limit to the number of rows returned by a query.

.validate_limit <- function(limit) {
  assert_that(is.null(limit) || (is.numeric(limit) & limit > 0),
              msg = "gss_hierarchy needs either NULL or a positive integer as parameter limit")

  return(limit)
}


#===============================================================================


# Validate a vintage and coerce to date object
#
# Given a vintage as an integer year, string or date, coerces to a date, setting
# month and day to Dec 31 if missing (this is the standard date that GSS
# vintages refer to). Also checks a gss vintage is for a vaguely reasonable year
# (1950 - 2049). If NULL, it returns NULL.

.validate_vintage_and_reformat <- function(vintage, return = "date") {

  if(is.null(vintage)) {
    return(NULL)
  }

  assert_that(return %in% c("character", "date"))

  assert_that(is.numeric(vintage) | is.character(vintage) | is.date(vintage),
              msg = "A gss vintage must be interpretable as a year or date")

  if(is.numeric(vintage)) {
    assert_that(all(.coercible_to_integer(vintage)))
    vintage <- paste(vintage, "12", "31", sep="-") %>% as.Date()
  }

  if(is.character(vintage)) {
    if(nchar(vintage) == 4) {
      vintage <- paste(vintage, 12, 31, sep="-") %>% as.Date()
    }
    if(nchar(vintage) == 7) {
      vintage <- paste(vintage, 31, sep=substr(vintage, 5, 5)) %>% as.Date()
    }
  }

  assert_that(all(vintage >= as.Date("1950-01-01")),
              all(vintage <= as.Date("2049-12-31")),
              msg = "Vintages must be for the years 1950 - 2049")

  if(return == "character") vintage <- as.character(vintage)

  return(vintage)
}

#===============================================================================

# Validate and reformat standardised resolutions to include in output

# Take a vector of resolutions of geographic data and standardise their format.
# Deals with any vintage of the format RESOLUTION + YEAR + CODE with the last
# two optional. Resolution is either a standardised name or code (plus a couple
# of special cases) e.g. LSOA, E01, though these are not validated (other
# functions will fail informatively when the API can't help them). Year is a
# two-digit year of the vintage. When it is not provided it will not be
# estimated. Code is either CD or NM. When it is not provided two elements will
# be returned for the one input, one with NM and one wth CD suffixed. So don't
# use this on a data frame (it is written with the expectation that these will
# be column names in a later output.) Returns NULL when NULL is input. If a
# vintage is provided the column inputs are checked to match the vintage.

# TODO should some vintages be corrected? e.g. LSOA18 == LSOA11
.validate_col_return_and_reformat <- function(col_return, vintage = NULL) {
  if(is.null(col_return)) {
    return(NULL)
  }

  assert_that(is.character(col_return),
              msg = "gss_hierarchy needs either NULL or a character vector as parameter col_return")

  # identify names vs gss codings (LSOA vs E01)
  is_coding <- substr(col_return, 2, 3) %>%  # this will be letters in a name, digits in a coding
    .coercible_to_integer()
  is_named <- !is_coding

  # make a data frame of the roots (E01/LSOA), and any suffixes (11/CD/NM)
  col_split <- data.frame(
    root = substr(col_return, 1, 3 + is_named),
    suff1 = substr(col_return, 4 + is_named, 5 + is_named),
    suff2 = substr(col_return, 6 + is_named, 7 + is_named),
  )

  col_split <- col_split %>%
    mutate(is_vintage = .coercible_to_integer(suff1),
           vintage = ifelse(is_vintage, suff1, ""),
           code = ifelse(is_vintage, suff2, suff1)) %>%
    select(root, vintage, code)

  remainder <- substring(col_return, 8 + is_named)

  assert_that(all(col_split$code %in% c("", "CD", "NM")),
              all(remainder == ""),
              msg = paste("gss_hierarchy requires col_return to be a character vector",
                          "with entries of the form E01, E01NM, E01CD, E0118,",
                          "E0118NM, E0118CD, LSOA, LSOANM, LSOACD, LSOA18CD, LSOA18NM",
                          "- where E01/LSOA can be substuted for the gss code root or name."))
  # TODO make this a better error message

  col_vintage_values <- unique(col_split$vintage[is_vintage])
  assert_that(length(col_vintage_values) <= 1,
              msg = paste(c("gss_hierarchy found multiple vintages specified in the col_return parameter:",
                            col_vintage_values), collapse = " "))

  if(length(col_vintage_values) == 0 & !is.null(vintage)) {
    assert_that(substring(input_vintage, 3, 4) == col_split$vintage,
                msg = paste(c("gss_hierarchy was given different vintage years in the vintage and the col_return parameters.",
                              "\nVintage:", vintage,
                              "\nCol return:", col_return), collapse = " "))
  }

  # We include the vintage year if the requested vintage is for the 31st December, otherwise we leave it out
  input_vintage <- .validate_vintage_and_reformat(vintage)
  exclude_vintage <- is.null(vintage) | substring(input_vintage, 6, 10) != "12-31"
  if(exclude_vintage) {
    col_split$vintage <- ""
  }

  # Complete the data frame with NM and CD suffixes if none were provided
  # TODO maybe use tidyr::complete here?
  ix <- col_split$code == ""
  col_split <- data.table::rbindlist(list(
    col_split %>% filter(ix) %>% mutate(code = "NM"),
    col_split %>% filter(ix) %>% mutate(code = "CD"),
    col_split %>% filter(!ix)))

  col_split <- mutate(col_split, out = paste0(root, vintage, code))
  return(col_split$out)
}

