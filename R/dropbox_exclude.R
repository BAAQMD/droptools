#' Exclude file(s) or folder(s) from Dropbox
#'
#' @param \dots path components (as in [file.path()])
#'
#' @export
dropbox_exclude <- function (...) {
  if (interactive()) {
    if (getwd() != my_dropbox()) {
      message("It looks like you're not in your Dropbox directory.")
      message("Did you mean to type setwd(my_dropbox()) first?")
      response <- readline("[Y/n] ")
      if (response != "n")
        old_wd <- setwd(my_dropbox())
    }
  }
  for (fn in list(...)) {
    dropbox_py("exclude", "add", str_c("\"", fn, "\""))
  }
  if (exists("old_wd"))
    setwd(old_wd)
}
