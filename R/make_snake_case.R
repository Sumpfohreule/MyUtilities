#' Converts a string from camelCase to snake_case
#'
#' @param string A string or vector which should be converted to camel_case
#' @return The string converted to snake_case
#' @export
make_snake_case <- function(camel_case_string) {
    camel_case_string %>%
        # Replace all capital letters with an underscore and its lower case
        gsub(pattern = "([A-Z0-9]+)",
             replacement = "_\\L\\1",
             x = .,
             perl = TRUE) %>%
        # Remove potential underscore from the beginning
        sub(pattern = "^_",
            replacement = "",
            x = ., perl = TRUE)
}
