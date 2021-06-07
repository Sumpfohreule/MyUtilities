########################################################################################################################
#' S3 Method for filling date times in a POSIXct vector
#'
#' @param date.time.vector A vector of POSIXct values
#' @param interval.seconds Target interval between two values in seconds
#' @export
#'
fillDateGaps.POSIXct <- function(date.time.vector, interval.seconds = calculateMainInterval(date.time.vector)) {
    .Deprecated("MyUtilities::fillGaps")
    return(seq(min(date.time.vector), max(date.time.vector), interval.seconds))
}
