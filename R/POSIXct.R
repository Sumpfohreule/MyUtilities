########################################################################################################################
#' Create an empty POSIXct vector
#'
#' A vector of a given length is created, which is of class POSIXct. Can be used to initialize empty data.frames
#' @param length The length of the vector to be created.
#' @export
#'
POSIXct <- function(length = 0) {
  return(.POSIXct(character(length)))
}
