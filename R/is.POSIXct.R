#' Checks if given object is of class POSIXct
#'
#' @param x An object to be checked
#' @export
#'
is.POSIXct <- function(x) {
  .Deprecated("lubridate::is.POSIXct")
  x_classes <- class(x)
  return(length(x_classes) == 2 &&
    x_classes[1] == "POSIXct" &&
    x_classes[2] == "POSIXt")
}
