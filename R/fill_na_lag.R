#' Fills NAs within a vector with the last proper value, which came before it
#'
#' NAs which start before any value are left as is.
#' The same goes for vectors only containing NA
#'
#' @param values Any vector
#' @export
fill_na_lag <- function(values) {
    is_na <- is.na(values)
    value_index <- which(!is_na)
    na_index <- which(is_na)
    if (all(length(value_index) != 0, length(na_index) != 0)) {
        na_index <- na_index %>%
            purrr::keep(~ .x > min(value_index))
        replacement_index <-  na_index %>%
            purrr::map(~ max(value_index[value_index < .x])) %>%
            unlist()

        values[na_index] <- values[replacement_index]
    }
    return(values)
}