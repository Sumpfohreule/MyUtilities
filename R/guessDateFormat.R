########################################################################################################################
#' Guess date format string
#'
#' Guesses the format string of a given date string for easier conversion to Date or POSIXct
#'
#' @param date.string A string containing a date
#' @export
#'
guessDateFormat <- function(date.string) {
    if (stringr::str_detect(date.string, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}.*$")) {
        date.format <- "%Y-%m-%d"
    } else if (stringr::str_detect(date.string, pattern = "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}.*$")) {
        date.format <- "%d.%m.%Y"
    } else if (stringr::str_detect(date.string, pattern = "^[0-9]{1,2} [[:alpha:]]{3} [0-9]{4}.*$")) {
        # TODO: make it work with different locals
        date.format <- "%d %b %Y"
    } else {
        stop("It was not possible to guess the date format")
    }
    if (stringr::str_detect(date.string, pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}$"))  {
        time.format <- "%H:%M:%S"
    } else if (stringr::str_detect(date.string, pattern = "[^:][0-9]{2}:[0-9]{2}$")) {
        time.format <- "%H:%M"
    } else {
        time.format <- NULL
    }
    return(trimws(paste(date.format, time.format)))
}
