#' Fills a data.frame with missing dates
#'
#' For each grouping column variation defined by group_columns the specific date
#' intervall is calculated and missing dates are filled into the data.frame
#' @param data A data.frame containing at least a date column
#' @param group_columns Optional: If the data is grouped, provide the grouping
#' column names
#' @param date_column Name of the date column. Can be omitted if the data.frame
#' contains a single column of type POSIXct
#' @param window If intervals can change within their groups use a window
#' ("year", "month", "day")
#' to specify a finer net to calculate the filling intervals
#' @export
fillDateGaps.data.frame <- function(data, group_columns = NULL,
                                    date_column = NULL, window = NULL) {
  assertthat::assert_that(assertthat::is.string(date_column) ||
    is.null(date_column))
  data <- as.data.table(data)

  if (is.null(date_column)) {
    date_column <- data %>%
      purrr::map(class) %>%
      purrr::map(function(x) "POSIXt" %in% x) %>%
      purrr::keep(function(x) x) %>%
      names()
  }
  if (length(date_column) < 1) {
    stop(paste0(
      "No date column found. Provide a date_column or convert the ",
      "column to POSIXct"
    ))
  } else if (length(date_column) > 1) {
    stop(paste0(
      "Multiple date columns found. Explicitly state the one to use ",
      "in date_column"
    ))
  }

  window_options <- c("year", "month", "yday")
  if (!is.null(window) && (window %in% window_options)) {
    additional_groups <- window_options[1:which(window_options %in% window)]
    for (new_group in additional_groups) {
      window_function <- get(new_group)
      data[, (new_group) := window_function(get(date_column))]
    }
    group_columns <- c(group_columns, additional_groups)
  }

  key_vector <- c(group_columns, date_column)
  setkeyv(data, key_vector)

  if (is.null(group_columns)) {
    new_dates <- data[, .(dates = fillDateGaps(get(date_column)))]
  } else {
    new_dates <- data[, .(dates = fillDateGaps(get(date_column))),
      by = c(group_columns)
    ]
  }
  setnames(new_dates,
    old = "dates",
    new = date_column
  )
  setkeyv(new_dates, key_vector)

  filled_data <- merge(new_dates, data, by = key_vector, all.x = TRUE)
  filled_data <- filled_data %>%
    select(-additional_groups)
  return(filled_data)
}
