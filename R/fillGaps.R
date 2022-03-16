#' Fills gaps between values with a given interval
#'
#' If values are provided with changing direction (ascending and descending order)
#' gaps are filled accordingly. Make sure values are ordered beforehand if only
#' a single orientation is expected
#'
#' \code{values} can be of any type that can be sequenced with \code{seq}.
#' (numeric, date, POSIXct, ...)
#'
#' @param values vector of values to to gap fill
#' @param interval A numeric value describing the expected interval
#' @export
fillGaps <- function(values, interval) {
  stop("Not working correctly yet!")
  assertthat::assert_that(length(interval) == 1)
  local_extrema <- .findLocalExtrema(values)
  purrr::map2(
    .x = values[local_extrema][-length(local_extrema)],
    .y = lead(values[local_extrema])[-length(local_extrema)],
    ~ .fillGaps(c(.x, .y), interval)
  ) %>%
    purrr::reduce(~ c(.x, .y[-1]))
}

.findLocalExtrema <- function(values) {
  c(1, which(diff(diff(values) > 0) != 0) + 1, length(values))
}

.fillGaps <- function(values, interval) {
  first_value <- head(values, 1)
  last_value <- tail(values, 1)
  if (first_value > last_value && interval > 0) {
    interval <- interval * -1
  }
  seq(first_value, last_value, by = interval)
}
