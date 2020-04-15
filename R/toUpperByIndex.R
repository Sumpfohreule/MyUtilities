#' Converts a single character to uppercase
#'
#' @param string A single character string or vector
#' @param index The character to be replaced
#' @export
#'
toUpperByIndex <- function(string, index) {
    character <- substring(string, first = index, last = index)
    substring(string, first = index, last = index) <- toupper(character)
    return(string)
}
