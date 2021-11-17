#' A purrr like function to keep only elements with certain names
#'
#' @param .x A list or atomic vector
#' @param .p A predicate function similar to purrr::keep.
#' Only element names which evaluate to TRUE with this function will be kept
#' @param ... Additional parameters passed to .p
#' @export
#'
keep_names <- function(.x, .p, ...) {
    keep_vector <- names(.x) %>%
        purrr::map_lgl(.p, ...)
    .x[keep_vector]
}
