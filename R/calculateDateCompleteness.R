########################################################################################################################
# FIXME: make usage for multiple years possible
# FIXME: extend.to.full.year has no effect
# TODO: merge with analyzeDateGaps (and remove return value)
#' Calculate the ration of existing dates
#'
#' The ratio between exisiting dates devided by the whole period is calculated to give on overview of its completeness
#'
#' @param dates A vector of POSIXct dates
#' @param interval The interval in seconds between two dates, if there are no gaps.
#' @param extend.to.full.year Flag which determines if the vector is supposed to start/end on the first/last day of
#' the year
#' @export
#'
calculateDateCompleteness <- function(dates, interval = NULL, extend.to.full.year = TRUE) {
    dates <- dates[order(dates)]
    interval.table <- table(diff(dates))
    if (TRUE %in% (as.numeric(attr(interval.table, "names")) <= 0)) {
        stop("'dates' must consist of (unique) strictly monotonically increasing values.")
    }
    if (is.null(interval)) {
        interval <- calculateMainInterval(dates)
    }
    years <- unique(data.table::year(dates))
    if (extend.to.full.year) {
        start.date <- as.POSIXctFixed(paste0(years, "-01-01"), tz = "UTC")
        end.date <- as.POSIXctFixed(paste0(years + 1, "-01-01"), tz = "UTC") - interval
        if (dates[1] != start.date) {
            dates <- c(start.date, dates)
        }
        if (tail(dates, n = 1L) != end.date) {
            dates <- as.POSIXctFixed(c(dates, end.date), tz = "UTC")
        }
        attr(dates, "tzone") <- "UTC"
    }
    min_date <- min(dates)
    max_date <- max(dates)
    full_date_sequenz <- seq(min_date, max_date, interval)

    date_length_ratio <- length(dates) / length(full_date_sequenz)
    date_length_percent <- round(date_length_ratio * 100)
    return(date_length_percent)
}
