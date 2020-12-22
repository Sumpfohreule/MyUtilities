########################################################################################################################
#' Efficient regex remaping of factors
#'
#' Convenient function for enaming of factor levels (and values) in an efficient way compared to "naive" way
#' (O(1+k) instead of O(n+k) I believe).
#'
#' @param factor_vector A factor vector
#' @param pattern A string regex pattern
#' @param replacement A string to replace the pattern with
#' @param keep_levels A boolean determining if original levels should be kept
#' @seealso \code{\link[stringr]{str_replace}} Used to change the levels (instead of the whole vector)
#' @export
#'
remapLevels <- function(factor_vector, pattern, replacement, keep_levels = TRUE) {
    factor_vector <- factor(factor_vector)
    new_levels <- stringr::str_replace(levels(factor_vector), pattern = pattern, replacement = replacement)
    if (keep_levels) {
        new_levels <- union(levels(factor_vector), new_levels)
    }
    levels(factor_vector) <- new_levels
    return(factor_vector)
}
