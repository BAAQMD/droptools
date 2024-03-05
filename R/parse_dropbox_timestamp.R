#' Parse timestamp(s) from the Dropbox API
#'
#' @param x timestamps to be parsed (character)
#' @param UTC force UTC? (logical)
#'
#' @note If `UTC` is `FALSE`, uses `Sys.timezone()` instead
#'
#' @importFrom lubridate hm fast_strptime with_tz
#' @importFrom stringr str_match
#'
#' @export
parse_dropbox_timestamp <- function (x, UTC = FALSE) {

  ts_regex <- "([0-9]{2} [A-Za-z]{3} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}) ([-+]?[0-9]{2})([0-9]{2})"
  ts_matched <- stringr::str_match(x, ts_regex)

  ts_tz_HH <- ts_matched[, 3]
  ts_tz_MM <- ts_matched[, 4]
  dt_offset <- lubridate::hm(paste0(ts_tz_HH, ":", ts_tz_MM))

  ts_datetime <- ts_matched[, 2]
  ts_datetime_format <- "%d %b %Y %H:%M:%S"
  dt_naive <- lubridate::fast_strptime(ts_datetime, format = ts_datetime_format)

  dt_UTC <- dt_naive + dt_offset

  if (isTRUE(UTC)) {
    dt_result <- dt_UTC
  } else {
    dt_local <- lubridate::with_tz(dt_UTC, Sys.timezone())
    dt_result <- dt_local
  }

  return(as.POSIXct(dt_result))

}
