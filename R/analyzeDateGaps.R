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
    interval.table <- dates %>%
        diff() %>%
        table()

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
    gap.indices <- which((diff(dates) - lubridate::make_difftime(interval, units = "second")) >= interval)
    total_measurements <- (max(lubridate::seconds(dates)) - min(lubridate::seconds(dates))) / interval + 1
    missing_measurements <- ((as.numeric(dates[gap.indices + 1]) - interval) - (as.numeric(dates[gap.indices]) + interval)) / interval + 1
    gap_size_percent <- round(missing_measurements / total_measurements * 100)

    gap.table <- data.frame(
        gap_start = dates[gap.indices],
        gap_end = dates[gap.indices + 1],
        gap_size_percent = gap_size_percent)
    return(gap.table)
}