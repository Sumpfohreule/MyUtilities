########################################################################################################################
#' Calculating the number of days of a given date
#'
#' @description This function calculates the number of days of a month, based on a given POSIXct value.
#' Leap years should be handled correctly.
#' 
#' @param date A single POSIXct value for which the number of days should be calculated
#' @keywords dates
#' @export 
#' @examples
#' # febuary of a leap year (29 days)
#' daysOfMonth(as.POSIXctFixed("2016-02-01", tz = "UTC"))
#' 
#' # febuary of a non leap year
#' daysOfMonth(as.POSIXctFixed("2017-02-01", tz = "UTC"))
#' 
daysOfMonth <- function(date) {
  year <- data.table::year(date)
  month <- data.table::month(date)
  start <- as.POSIXctFixed(paste0(year, "-", month, "-01"), format = "%Y-%m-%d", tz = "UTC")
  end <- as.POSIXctFixed(paste0(year, "-", month + 1, "-01"), format = "%Y-%m-%d", tz = "UTC")
  return(as.integer(end - start))
}
