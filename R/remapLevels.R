########################################################################################################################
#' Efficient regex remaping of factors
#'
#' Convenient function for enaming of factor levels (and values) in an efficient way compared to "naive" way
#' (O(1+k) instead of O(n+k) I believe).
#'
#' @param factor_vector A factor vector
#' @param A string regex pattern
#' @param A string to replace the pattern with
#' @seealso \code{\link[stringr]{str_replace}} Used to change the levels (instead of the whole vector)
#' @export
#'
remapLevels <- function(factor_vector, pattern, replacement) {
    factor_vector <- factor(factor_vector)
    levels(factor_vector) <- stringr::str_replace(levels(factor_vector), pattern = pattern, replacement = replacement)
    return(factor_vector)
}
