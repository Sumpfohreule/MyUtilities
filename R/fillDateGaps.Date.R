########################################################################################################################
#' S3 Method for filling date times in a Date vector
#' 
#' @param date.vector A vector of Date values
#' @param interval.days Target interval between two values in days
#' @export 
#' 
fillDateGaps.Date <- function(date.vector, interval.days = calculateMainInterval(date.vector)) {
    return(seq(min(date.vector), max(date.vector), interval.days))
}
