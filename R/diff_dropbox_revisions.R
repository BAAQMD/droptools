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
#' @importFrom stringr str_c
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
diff_dropbox_revisions <- function (path, then, now = NULL) {

  data_now <- get_dropbox_data(path, as_of = now)
  data_then <- get_dropbox_data(path, as_of = then)

  label_for <- function (x) {
    metadata <- attr(x, "dropbox_metadata")
    mod_ts <- metadata[["modified"]]
    mod_name <- metadata[["modifier.display_name"]]
    stringr::str_c(mod_ts, "&nbsp;(", mod_name, ")")
  }

  label_then <- str_c(label_for(data_then), "&nbsp;")
  label_now <- if (is.null(now)) {
    "&nbsp;now"
  } else {
    str_c("</br>", label_for(data_now))
  }

  standardize_blanks <- function (x) {
    trimmed <- stringr::str_trim(x)
    dplyr::na_if(trimmed, "")
  }

  diffed <- diff_data(
    data_then %>% mutate_if(is_character, standardize_blanks),
    data_now %>% mutate_if(is_character, standardize_blanks))

  render_diff(diffed, title = str_c(label_then, "&rarr;", label_now))

}
