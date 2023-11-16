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

  path_options <- list(
    file.path("~", "BAAQMD Dropbox"),
    file.path("~", "Dropbox (BAAQMD)"),
    file.path("~", "Dropbox"))

  for (path in path_options) {
    abs_path <- normalizePath(path)
    if (dir.exists(abs_path)) {
      return(abs_path)
    }
  }

  stop("Can't find a Dropbox directory")

}
