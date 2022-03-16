#' Wrapper for min which does remove NAs
#'
#' Returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply min() to
#' @param default Value to return in case \code{x} contains only NAs (default is NA)
#' @return Either the minimum value of the vector or the provided default value
#' @seealso \code{\link{max_with_default}, \link{mean_with_default}, \link{sum_with_default}}
#' @export
min_with_default <- function(x, default = as.numeric(NA)) {
  .aggregate_with_default(x, default, min)
}

#' Wrapper for max which does remove NAs
#'
#' Returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply max() to
#' @param default Value to return in case \code{x} contains only NAs (default is NA)
#' @return Either the maximum value of the vector or the provided default value
#' @seealso \code{\link{min_with_default}, \link{mean_with_default}, \link{sum_with_default}}
#' @export
max_with_default <- function(x, default = as.numeric(NA)) {
  .aggregate_with_default(x, default, max)
}

#' Wrapper for mean which does remove NAs
#'
#' Returns \code{default} instead of NaN in case all values are NAs
#' @param x value vector to apply mean() to
#' @param default Value to return in case \code{x} contains only NAs (default is NA)
#' @return Either the mean value of the vector or the provided default value
#' @seealso \code{\link{min_with_default}, \link{max_with_default}, \link{sum_with_default}}
#' @export
mean_with_default <- function(x, default = as.numeric(NA)) {
  .aggregate_with_default(x, default, mean)
}

#' Wrapper for sum which does remove NAs
#'
#' This function has been implemented mostly so it can be used like the min, max mean versions as 0 is already returned by sum as default
#' @param x value vector to apply sum() to
#' @param default Value to return in case \code{x} contains only NAs (default is NA)
#' @return Either the sum value of the vector or the provided default value
#' @seealso \code{\link{min_with_default}, \link{max_with_default}, \link{mean_with_default}}
#' @export
sum_with_default <- function(x, default = as.numeric(NA)) {
  .aggregate_with_default(x, default, sum)
}

.aggregate_with_default <- function(x, default, aggregate_function) {
  if (any(is.finite(x))) {
    return(aggregate_function(x, na.rm = TRUE))
  } else {
    return(default)
  }
}
