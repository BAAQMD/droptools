#' Diff a CSV file's history
#'
#' @description Compare two revisions of data stored in a .CSV file on Dropbox
#'
#'
#' @param path relative to ~/Dropbox
#' @param \dots path components (as in [file.path()])
#' @param then first datetime
#' @param now second datetime (defaults to [Sys.time()])
#'
#' @note DOES NOT YET WORK ON RSTUDIO SERVER. WILL SOMEDAY SOON.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange mutate mutate_if desc as_tibble
#' @importFrom stringr str_c str_trim
#' @importFrom daff diff_data render_diff
#'
#' @examples
#' \dontrun{
#' library(inventory)
#'
#' # Pick a file
#' csv_path <-
#'   my_dropbox("BY2015", "Throughputs",
#'              "BY2015-throughputs-area_sources.csv")
#'
#' # How has this file changed in the last two weeks?
#' diff_dropbox_revisions(
#'   csv_path,
#'   now = Sys.time(),
#'   then = Sys.time() - ddays(14))
#'
#' # You don't actually have to supply `now` ---
#' # but if you don't, you do have to say `then = `
#' diff_dropbox_revisions(
#'   csv_path,
#'   then = Sys.time() - ddays(14))
#'
#' # You can diff two known revisions
#' # that differ by as little as a few mminutes
#' t1 <- "2017-09-08 09:10:00 PDT"
#' t2 <- "2017-09-08 10:10:00 PDT"
#' diff_dropbox_revisions(csv_path, t1, t2)
#'
#' }
#'
#' @seealso [ddays()], [dhours()], etc. (from the `lubridate` package)
#' @seealso [render_diff()] and [diff_data()]
#'
#' @export
diff_dropbox_revisions <- function (
  path,
  then,
  now = NULL,
  ...
) {

  now_data <- get_dropbox_data(path, as_of = now, ...)
  then_data <- get_dropbox_data(path, as_of = then, ...)

  # This can be either "client_modified" or "server_modified".
  mod_var <- "client_modified"

  html_label_for <- function (x) {
    metadata <- attr(x, "dropbox_metadata")
    mod_ts <- metadata[[mod_var]]
    return(mod_ts)
    #mod_name <- metadata[["sharing_info.modified_by"]]
    #stringr::str_c(mod_ts, "&nbsp;(", mod_name, ")")
  }

  label_then <-
    html_label_for(then_data) %>%
    stringr::str_c("&nbsp;")

  label_now <- if (is.null(now)) {
    "&nbsp;now"
  } else {
    str_c("</br>", html_label_for(now_data))
  }

  standardize_blanks <- function (x) {
    trimmed <- stringr::str_trim(x)
    dplyr::na_if(trimmed, "")
  }

  diff_object <-
    daff::diff_data(
      then_data,
      now_data)

  daff::render_diff(
    diff_object,
    title = stringr::str_c(label_then, "&rarr;", label_now))

  return(invisible(diff_object))

}
