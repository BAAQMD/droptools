#' Get history from Dropbox API
#'
#' @param path relative to ~/Dropbox
#' @param dtoken passed to `rdrop2` package methods
#' @param rev_limit passed to `rdrop2` package methods
#'
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange mutate mutate_if desc bind_rows
#'
#' @include parse_dropbox_timestamp.R
#'
#' @export
get_dropbox_history <- function (path = NULL, dtoken = NULL, rev_limit = 1000) {

  if (is.null(dtoken)) dtoken <- rdrop2:::get_dropbox_token()

  rev_url <- paste0(
    "https://api.dropbox.com/1/revisions/auto/",
    rdrop2:::strip_slashes(path))

  query <- list(rev_limit = rev_limit)
  response <- httr::GET(rev_url, config = config(token = dtoken), query = query)
  httr::stop_for_status(response)
  results <- httr::content(response)

  res_df <- lapply(results, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))

  # WAS: combined <- data.table::rbindlist(res_df, fill = TRUE) %>% as_tibble()
  combined <- as_tibble(bind_rows(res_df))

  parsed <- mutate(combined, modified = parse_dropbox_timestamp(modified))
  arrange(parsed, desc(modified))

  #select(revision, modified, client_mtime, modifier.display_name, bytes, path, everything())
}
