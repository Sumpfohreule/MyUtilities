#' Drops values from a vector which are more than 2x sd away from the mean value
#'
#' If present, values outside of absoluteMin and absoluteMax are dropped first.
#' Afterwards values are dropped, that
#' are more than 2x of the standard deviation away from the mean value.
#'
#' @param absolute_min The absolute minimum value which might still be okay.
#' Lower values are dropped
#' @param absolute_max The absolute maximum value which might still be okay.
#' Higher values are dropped
#' @export
#'
dropExtremValues <- function(values, absolute_min = NULL, absolute_max = NULL) {
  if (!is.null(absolute_min)) {
    values[values < absolute_min] <- NA
  }
  if (!is.null(absolute_max)) {
    values[values > absolute_max] <- NA
  }
  mean.value <- mean(values, sd.rm = TRUE)
  sd.value <- sd(values, na.rm = TRUE)
  values[values > mean.value + sd.value * 2] <- NA
  values[values < mean.value - sd.value * 2] <- NA
  return(values)
}
