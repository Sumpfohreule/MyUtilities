########################################################################################################################
#' Find the most prevalent interval in a vector
#'
#' The interval between all neighbouring vector values is calculated and the most frequent one returned.
#' Can be used to find the actual logging interval between dates if there are gaps in the vector
#'
#' @param vector A vector of values which must be convertible to numeric (dates, date-times, etc.)
#' @export
#'
calculateMainInterval <- function(vector) {
    assertthat::assert_that(length(vector) > 1)
    vector <- as.numericTryCatch(vector)
    diff.counts <- table(diff(as.numeric(vector)))
    diff.value <- attr(diff.counts[diff.counts == max(diff.counts)], "names")
    if (length(diff.value) > 1) {
        stop("There are multiple intervals with the same frequency")
    }
    return(as.numeric(diff.value))
}
