min_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, min)
}

max_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, max)
}

mean_with_default <- function(x, default = 0) {
    .aggregate_with_default(x, default, mean)
}

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