#' dropbox_py
#'
#' @param \dots named arguments (character vector) passed to `dropbox.py`
#'
#' @description Send arbitrary stuff to dropbox.py commandline tool
#'
dropbox_py <- function (...) {

  path_to_py <- system("which dropbox.py", intern = TRUE)
  stopifnot(file.exists(path_to_py))

  py_args <- str_c(..., sep = " ") # FIXME: no sanity checking or validation here
  cmd <- str_c(path_to_py, " ", py_args)

  system(cmd)

}
