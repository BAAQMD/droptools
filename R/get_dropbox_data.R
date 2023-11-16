#' Get the version that existed at a certain time
#'
#' @param path relative to ~/Dropbox
#' @param dtoken passed to `rdrop2` package methods
#' @param as_of most recent revision prior to or including this POSIXct
#'
#' @importFrom magrittr %>%
#' @importFrom tools file_ext
#' @importFrom readr read_csv write_file
#' @importFrom tibble as_tibble
#' @importFrom stringr str_to_lower str_remove
#' @importFrom httr POST add_headers config stop_for_status content
#' @importFrom rdrop2 drop_read_csv
#' @importFrom lubridate ymd_hms
#' @importFrom readxl read_xlsx
#' @importFrom dplyr slice filter filter_at
#'
#' @export
get_dropbox_data <- function (
  path,
  as_of = NULL,
  sheet = 1,
  ...,
  dtoken = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[get_dropbox_data] ", ...)

  if (is.null(as_of)) {
    # Default to the "most recent" revision
    as_of <- Sys.time()
  } else if (is.character(as_of)) {
    # Convert `as_of` into a true datetime object
    as_of <- lubridate::ymd_hms(as_of, tz = Sys.timezone())
  }

  # This can be either "client_modified" or "server_modified".
  mod_var <- "client_modified"

  # Be nice (100 revs is as far back as we can get it)
  msg("fetching 10 revisions")
  recent_history <- get_dropbox_history(path, limit = 10)
  if (as_of < min(recent_history[[mod_var]])) {
    msg("fetching 100 revisions")
    recent_history <- get_dropbox_history(path, limit = 100)
    if (as_of < min(recent_history[[mod_var]])) {
      stop("Can't go that far back, sorry (rev_limit = 1000)")
    }
  }

  # Discard revisions that happened after `as_of`
  prior_history <-
    recent_history %>%
    filter_at(
      vars(client_modified),
      all_vars(. < as_of))

  # Take the "most recent" remaining record
  matching_metadata <-
    prior_history %>%
    filter_at(
      vars(client_modified),
      all_vars(. == max(.)))

  msg(mod_var, " is: ", matching_metadata[[mod_var]])

  matching_id <-
    pull(
      matching_metadata,
      id) %>%
    stringr::str_remove(
      "^id:")

  matching_rev <-
    pull(
      matching_metadata,
      rev) %>%
    stringr::str_remove(
      "^rev:")

  msg("id is: ", matching_id)
  msg("rev is: ", matching_rev)

  if (is.null(dtoken)) {
    dtoken <- rdrop2:::get_dropbox_token()
  }

  path_ext <-
    tools::file_ext(path) %>%
    str_to_lower()

  json_payload <-
    list(path = str_c("rev:", matching_rev)) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  response_object <-
    httr::POST(
      url = "https://content.dropboxapi.com/2/files/download",
      config = httr::config(
        token = rdrop2:::get_dropbox_token()),
      httr::add_headers(
        `Dropbox-API-Arg` = json_payload))
  #httr::verbose())

  httr::stop_for_status(response_object)
  response_content <- httr::content(response_object, as = "raw")
  msg("md5(content) is: ", digest::digest(response_content, algo = "md5"))
  response_path <- tempfile(fileext = str_c(".", path_ext))
  msg("writing raw content to ", response_path)
  readr::write_file(response_content, path = response_path)

  if (path_ext == "csv") {
    downloaded_data <-
      readr::read_csv(
        response_path,
        ...)
  } else if (path_ext == "xlsx") {
    downloaded_data <-
      readxl::read_xlsx(
        response_path,
        sheet = sheet,
        ...)
  }

  attr(downloaded_data, "dropbox_metadata") <- matching_metadata
  return(downloaded_data)

}
