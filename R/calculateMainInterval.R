########################################################################################################################
# FIXME: catch single value (and missing value / non POSIXct)
# TODO: Check how this function handles if multiple intervals are equaly frequent
#' Find the most prevalent interval in a vector
#' 
#' The intervall between all neighbouring vector values is calculated and the most frequent one returned.
#' Can be used to find the actual logging interval between dates if there are gaps in the vector
#' 
#' @param vector A vector of values which must be convertible to numeric (dates, date-times, etc.)
#' @export 
#' 
calculateMainInterval <- function(vector) {
    vector <- as.numericTryCatch(vector)
    diff.counts <- table(diff(as.numeric(vector)))
    diff.value <- attr(diff.counts[diff.counts == max(diff.counts)], "names")
    return(as.numeric(diff.value))
}
