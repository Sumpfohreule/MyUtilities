#' Wrapper for min which does remove NAs and returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply min() to
#' @param default Value to return in case \code{x} contains only NAs (default is 0)
#' @return Either the minimum value of the vector or the provided default value
#' @export
min_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, min)
}

#' Wrapper for max which does remove NAs and returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply max() to
#' @param default Value to return in case \code{x} contains only NAs (default is 0)
#' @return Either the maximum value of the vector or the provided default value
#' @export
max_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, max)
}

#' Wrapper for mean which does remove NAs and returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply mean() to
#' @param default Value to return in case \code{x} contains only NAs (default is 0)
#' @return Either the mean value of the vector or the provided default value
#' @export
mean_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, mean)
}

#' Wrapper for sum which does remove NAs and returns \code{default} instead of Inf, -Inf in case all values are NAs
#' @param x value vector to apply sum() to
#' @param default Value to return in case \code{x} contains only NAs (default is 0)
#' @return Either the sum value of the vector or the provided default value
#' @export
sum_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, sum)
}

.aggregate_with_default <- function(x, default, aggregate_function) {
    if (any(is.finite(x))) {
        return(aggregate_function(x, na.rm = TRUE))
    } else {
        return(default)
    }
}