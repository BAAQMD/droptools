#' Dropbox file/folder path(s)
#'
#' @description
#' Forms a path to file(s) or folders in your Dropbox.
#' Works just like `file.path("~", "Dropbox", ...)`
#'
#' @param \dots path components (as in [file.path()])
#' @param mustWork passed to normalizePath()
#' @param verbose logical
#'
#' @importFrom fs path path_home
#'
#' @note Looks for the following, in order:
#' - "BAAQMD Dropbox"
#' - "Dropbox (BAAQMD)"
#' - "Dropbox"
#'
#' Throws an error if none can be found.
#'
#' @seealso [file.path()]
#'
#' @export
my_dropbox <- function (..., mustWork = FALSE, verbose = FALSE) {

  home <- fs::path_home()

  path_options <- list(
    fs::path(home, "BAAQMD Dropbox"),
    fs::path(home, "Dropbox (BAAQMD)"),
    fs::path(home, "Dropbox"))

  for (path in path_options) {
    if (dir.exists(path)) {
      return(normalizePath(path))
    }
  }

  err_msg <- paste0(
    "Can't find a Dropbox directory under ",
    home)

  stop(err_msg)

}
