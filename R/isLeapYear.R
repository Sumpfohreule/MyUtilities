########################################################################################################################
#' Checks if a given year is a leap year
#' 
#' @description A simple function determining if a given numeric value is a leap year
#' 
#' @param year A numeric value which represents a year
#' @keywords dates
#' @export 
#' @examples 
#' # is leap year:
#' isLeapYear(1996)
#' 
#' # not leap year(1997)
#' isLeapYear(1997)
#' 
#' # not leap year (devisable by 100 but not 400)
#' isLeapYear(1900)
#' 
#' # is leap year (devisable by 400)
#' isLeapYear(2000)
#' 
isLeapYear <- function(year) {
    return (year %% 400 == 0 | (year %% 4 == 0 & !year %% 100 == 0))
}
