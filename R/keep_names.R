keep_names <- function(.x, .p, ...) {
    keep_vector <- names(.x) %>%
        purrr::map_lgl(.p, ...)
    .x[keep_vector]
}
