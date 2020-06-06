########################################################################################################################
# FIXME: make usage for multiple years possible
# TODO: merge with calculateDateCompleteness (and no return value)
#' Summarizing gap information within dates
#'
#' Gaps within a date vector are localized and information about start, end and size are provided
#'
#' @param dates A vector of POSIXct dates
#' @param interval The interval in seconds between two dates, if there are no gaps.
#' @param extend.to.full.year Flag which determines if the vector is supposed to start/end on the first/last day of
#' the year
#' @export
#'
analyzeDateGaps <- function(dates, interval = NULL, extend.to.full.year = FALSE) {
    dates <- dates[order(dates)]
    interval.table <- table(diff(dates))
    if (TRUE %in% (as.numeric(attr(interval.table, "names")) <= 0)) {
        stop("'dates' must consist of (unique) strictly monotonically increasing values.")
    }
    if (is.null(interval)) {
        interval <- calculateMainInterval(dates)
    }
    if (extend.to.full.year) {
        years <- unique(data.table::year(dates))
        start.date <- as.POSIXctFixed(paste0(min(years), "-01-01"), tz = "UTC")
        end.date <- as.POSIXctFixed(paste0(max(years) + 1, "-01-01"), tz = "UTC") - interval
        if (dates[1] != start.date) {
            dates <- c(start.date, dates)
        }
        if (tail(dates, n = 1L) != end.date) {
            dates <- as.POSIXctFixed(c(dates, end.date), tz = "UTC")
        }
        attr(dates, "tzone") <- "UTC"
    }
    interval.minutes <- interval / 60
    gap.indices <- which( (diff(dates) - interval.minutes) >= interval.minutes)
    minutes_total <- (max(as.numeric(dates)) - min(as.numeric(dates))) / 60
    gap.table <- data.table::data.table(
        gap.start = dates[gap.indices],
        gap.end = dates[gap.indices + 1],
        gap.size.percent = round(as.numeric(diff(dates)[gap.indices] - interval.minutes) / minutes_total * 100))
    return(gap.table)
}
