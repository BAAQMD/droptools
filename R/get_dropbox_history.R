#' Get history from Dropbox API
#'
#' @param path relative to ~/Dropbox
#' @param dtoken passed to `rdrop2` package methods
#' @param rev_limit passed to `rdrop2` package methods
#'
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_at
#' @importFrom purrr map_df
#' @importFrom readr parse_datetime
#'
#' @include parse_dropbox_timestamp.R
#'
#' @export
get_dropbox_history <- function (
  path,
  limit = 100
) {

  rev_list <-
    rdrop2:::drop_list_revisions(
      path = path,
      limit = limit)

  rev_data <-
    rev_list %>%
    .[["entries"]] %>%
    purrr::map_df(
      ~ as.list(unlist(.))) %>%
    dplyr::mutate_at(
      vars(client_modified, server_modified),
      readr::parse_datetime)

  return(rev_data)

}
