#' Adding first and last date of a year to a POSIXct vector
#'
#' Depending on the paramters, the first and last POSIXct are added to a POSIXct
#' vector.
#' The interval, which is important for the last date of a year can be provided
#' (default: calculated from the vector)
#'
#' @param dates A POSIXct vector
#' @param interval The interval between to POSIXct values in seconds
#' @param add.start Flag if year start should be added. A date can be provided
#' instead of the years beginning
#' @param add.end Flag if year start should be added. A date can be provided
#' instead of the years ending
#' @export
#'
addYearStartEnd <- function(dates,
                            interval = calculateMainInterval(dates),
                            add.start = TRUE,
                            add.end = TRUE) {
  interval # Needed, to prevent lazy evaluation
  if (add.start == TRUE) {
    start.year <- min(data.table::year(dates))
    start.date <- as.POSIXct(paste0(start.year, "-01-01 00:00:00"), tz = "UTC")
  } else if (class(add.start) %in% c("character", "POSIXct")) {
    start.date <- as.POSIXct(add.start, tzone = "UTC")
  }
  if (add.start != FALSE && min(dates) != start.date) {
    dates <- c(start.date, dates)
    attr(dates, "tzone") <- "UTC"
  }
  if (add.end == TRUE) {
    end.year <- max(data.table::year(dates))
    end.date <- as.POSIXct(
      paste0(end.year + 1, "-01-01 00:00:00"),
      tz = "UTC"
    ) - interval
  } else if (class(add.end) %in% c("character", "POSIXct")) {
    end.date <- as.POSIXct(add.end, tzone = "UTC")
  }
  if (add.end != FALSE && max(dates) != end.date) {
    dates <- c(dates, end.date)
    attr(dates, "tzone") <- "UTC"
  }
  return(dates)
}
