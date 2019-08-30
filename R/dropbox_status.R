#' Check Dropbox status
#'
#' @export
dropbox_status <- function () {
  dropbox_py("status")
}
