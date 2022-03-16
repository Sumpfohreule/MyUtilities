########################################################################################################################
# TODO: make fillDateGaps.default a real default method or maybe just use seq to overload
#' Generic S3 Method for filling in date times
#'
#' Missing value within a date/time vector are filled according to a provided interval
#'
#' @export
#'
fillDateGaps <- function(obj, ...) {
  UseMethod("fillDateGaps")
}

fillDateGaps.default <- function(obj) {
  .Deprecated("MyUtilities::fillGaps")
  stop(
    "Only S3 method implementations for the following classes exist:\n\t",
    paste0(methods(fillDateGaps), collapse = "\n\t")
  )
}
