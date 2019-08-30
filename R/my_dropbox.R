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
#' @note Looks first for "~/Dropbox (BAAQMD)". Failing that, looks for "~/Dropbox" instead. Throws an error if neither can be found.
#'
#' @seealso [file.path()]
#'
#' @examples
#' csv_path <-
#'   my_dropbox("BY2015", "Training", "ari", "02-input",
#'              "BY2015-throughputs-area_sources.csv")
#'
#' file.exists(csv_path)
#'
#' @export
my_dropbox <- function (..., mustWork = FALSE, verbose = FALSE) {

  # Prefer this one
  first_option <- normalizePath(file.path("~", "Dropbox (BAAQMD)"))

  # ... to this one
  second_option <- normalizePath(file.path("~", "Dropbox"))

  if (dir.exists(first_option)) {
    result <- file.path(first_option, ...)
  } else if (dir.exists(second_option)) {
    result <- file.path(second_option, ...)
  } else {
    stop("Can't find a Dropbox directory")
  }

  return(result)

}
