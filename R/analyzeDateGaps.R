# FIXME: make usage for multiple years possible
# TODO: merge with calculateDateCompleteness (and no return value)
#' Summarizing gap information within dates
#'
#' Gaps within a date vector are localized and information about start, end and
#' size are provided
#'
#' @param dates A vector of POSIXct dates
#' @param interval The interval in seconds between two dates, if there are no
#'  gaps.
#' @param extend.to.full.year Flag which determines if the vector is supposed to
#'  start/end on the first/last day of
#' the year
#' @export
#'
analyzeDateGaps <- function(dates, interval = NULL,
                            extend.to.full.year = FALSE) {
  if (length(dates) <= 1 && !extend.to.full.year) {
    empty_output <- data.frame(
      gap_start = lubridate::Date(),
      gap_end = lubridate::Date(),
      gap_size_percent = numeric()
    )
    return(empty_output)
  }
  dates <- dates[order(dates)] %>%
    lubridate::as_datetime()
  interval.table <- dates %>%
    diff() %>%
    table()

  if (TRUE %in% (as.numeric(attr(interval.table, "names")) <= 0)) {
    stop(paste0(
      "'dates' must consist of (unique) strictly monotonically ",
      "increasing values."
    ))
  }
  if (is.null(interval)) {
    interval <- calculateMainInterval(dates)
  }
  if (extend.to.full.year) {
    years <- unique(data.table::year(dates))
    start.date <- lubridate::as_datetime(paste0(min(years), "-01-01"))
    end.date <- lubridate::as_datetime(paste0(max(years) + 1, "-01-01"))
    -lubridate::seconds(interval)
    if (dates[1] != start.date) {
      dates <- c(start.date, dates)
    }
    if (tail(dates, n = 1L) != end.date) {
      dates <- c(dates, end.date)
    }
  }
  gap_indices <- which((diff(dates) - lubridate::make_difftime(
    num = interval,
    units = "second"
  )) >= interval)
  total_measurements <- (as.numeric(max(dates)) - as.numeric(min(dates))) /
    interval + 1
  missing_measurements <- ((as.numeric(dates[gap_indices + 1]) - interval) -
    (as.numeric(dates[gap_indices]) + interval)) /
    interval + 1
  gap_size_percent <- round(missing_measurements / total_measurements * 100)

  gap_table <- data.frame(
    gap_start = dates[gap_indices],
    gap_end = dates[gap_indices + 1],
    gap_size_percent = gap_size_percent
  )
  return(gap_table)
}
