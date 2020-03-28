#' Internal function used for clearing the tempdir() when testing
#' @export
.clear_tempdir <- function() {
    unlink(dir(tempdir(), full.names = TRUE), recursive = TRUE)
}
