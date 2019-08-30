context("parse_dropbox_timestamp")

x <- "06 Sep 2017 09:07:33 -0500"
parsed <- parse_dropbox_timestamp(x, UTC = TRUE)
expect_is(parsed, "POSIXct")
expected <- ISOdate(2017, 09, 06, 04, 07, 33, tz = "UTC")
expect_identical(parsed, expected)
