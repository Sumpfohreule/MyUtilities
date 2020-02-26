########################################################################################################################
#' Dropping alternating NA values
#'
#' This function drops rows, only if they are alternating between value and NA. This helps to remove single value gaps
#' without dropping large gaps
#'
#' @param value.dt A data.table
#' @param value.col The column containing the values to check for alternating NA/value
#' @export
#' @examples
#' value.dt <- data.table(
#'     times = c(0, 15, 30, 45, 60, 75, 90, 105, 120, 150, 180, 210, 240, 270, 300),
#'     values = c(1, NA, 2, NA, 3, NA, 4, NA, 5, 6, 7, NA, NA, 8, NA))
#' value.dt <- dropAlternatingNaValues(value.dt, value.col = "values")
#'
dropAlternatingNaValues <- function(value.dt, value.col) {
    copy.dt <- data.table::copy(value.dt)
    copy.dt[, ":=" (
            shift_lag = data.table::shift(get(value.col), type = "lag"),
            shift_lead = data.table::shift(get(value.col), type = "lead"))]
    out.dt <- copy.dt[!(is.na(get(value.col) & !is.na(shift_lag) & !is.na(shift_lead)))]
    out.dt[, ":=" (
            shift_lag = NULL,
            shift_lead = NULL)]
    return(out.dt)
}
