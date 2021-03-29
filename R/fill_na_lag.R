fill_na_lag <- function(values) {
    assertthat::assert_that(!all(is.na(values)), msg = "Can't fill if only NAs are contained")
    is_na <- is.na(values)
    value_index <- which(!is_na)
    na_index <- which(is_na)
    replacement_index <- na_index %>%
        purrr::map(~ max(value_index[value_index < .x])) %>%
        unlist()


    values[na_index] <- values[replacement_index]
    return(values)
}