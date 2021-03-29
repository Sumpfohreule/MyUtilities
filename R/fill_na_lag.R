fill_na_lag <- function(values) {
    browser()
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