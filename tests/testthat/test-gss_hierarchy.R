library(geograpi)
library(testthat)

# You'll need an internet connection to run these tests

# TODO I don't currently know the query limit for the API and haven't looked at
# a reliable way of catching errors

# TODO test oa, lsoa, msoa, (ward? work zone)
# TODO test awkward places (scilly, city)

input_oa <- data.frame(gss_code = c("E00000001", "E00000003"),
                       data = letters[1:2],
                       stringsAsFactors = FALSE)

input_lsoa <- data.frame(gss_code = c("E01000001", "E01000002"),
                         data = letters[1:2],
                         stringsAsFactors = FALSE)

input_msoa <- data.frame(gss_code = c("E02000001", "E02000002"),
                         data = letters[1:2],
                         stringsAsFactors = FALSE)

input_ward <- data.frame(gss_code = c("E05000001", "E05000015"),
                         data = letters[1:2],
                         stringsAsFactors = FALSE)

input_lad <- data.frame(gss_code = c("E06000001", "E06000001"),
                        data = letters[1:2],
                        stringsAsFactors = FALSE)





#
