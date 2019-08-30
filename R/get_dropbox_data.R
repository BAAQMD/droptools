#' Get the version that existed at a certain time
#'
#' @param path relative to ~/Dropbox
#' @param dtoken passed to `rdrop2` package methods
#' @param as_of most recent revision prior to or including this POSIXct
#'
#' @importFrom magrittr %>%
#' @importFrom tools file_ext
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_lower
#' @importFrom httr GET config stop_for_status content
#' @importFrom rdrop2 drop_read_csv
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr slice filter
#'
#' @export
get_dropbox_data <- function (path, as_of = NULL, dtoken = NULL) {

  # This function is only for use with .CSV data
  stopifnot(str_to_lower(file_ext(path)) == "csv")

  if (is.null(dtoken)) dtoken <- rdrop2:::get_dropbox_token()

  if (is.null(as_of)) {
    # Short-cut to the "current" revision
    csv_data <- suppressMessages(drop_read_csv(path, dtoken = dtoken, stringsAsFactors = FALSE))
    return(as_tibble(csv_data))
  }

  # Make `as_of` into a datetime, if necessary
  if (is.character(as_of)) {
    as_of <- ymd_hms(as_of, tz = Sys.timezone())
  }

  # Be nice (1000 revs is as far back as we can get it)
  recent_history <- get_dropbox_history(path, rev_limit = 10)
  if (as_of < min(recent_history[["modified"]])) {
    recent_history <- get_dropbox_history(path, rev_limit = 100)
    if (as_of < min(recent_history[["modified"]])) {
      recent_history <- get_dropbox_history(path, rev_limit = 1000)
      if (as_of < min(recent_history[["modified"]])) {
        stop("Can't go that far back, sorry (rev_limit = 1000)")
      }
    }
  }

  # Discard history that happened after `as_of`
  prior_history <-
    recent_history %>%
    arrange(desc(modified)) %>%
    filter(modified < as_of)

  # Take the first remaining metadata record
  dropbox_metadata <-
    prior_history %>%
    slice(1) %>%
    as.list()

  base_url <- "https://api-content.dropbox.com/1/files/auto/"
  query <- with(dropbox_metadata, as.list(rdrop2:::drop_compact(c(rev = rev))))
  full_download_path <- paste0(base_url, path)

  response <- GET(url = full_download_path, config = config(token = dtoken), query = query)
  stop_for_status(response)
  suppressMessages(csv_data <- content(response))

  attr(csv_data, "dropbox_metadata") <- dropbox_metadata
  return(csv_data)

}
