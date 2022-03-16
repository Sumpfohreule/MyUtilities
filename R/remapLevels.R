# Probably should be deprecated in favor of forcats::
#' Efficient regex remaping of factors
#'
#' Convenient function for renaming of factor levels (and values) in an
#' efficient way
#'
#' @param factor_vector A factor vector
#' @param pattern A string regex pattern
#' @param replacement A string to replace the pattern with
#' @param keep_levels A boolean determining if original levels should be kept
#' @seealso \code{\link[stringr]{str_replace}} Used to change the levels
#' (instead of the whole vector)
#' @export
#'
remapLevels <- function(factor_vector, pattern, replacement,
                        keep_levels = TRUE) {
  factor_vector <- factor(factor_vector)
  old_levels <- levels(factor_vector)
  new_levels <- stringr::str_replace(old_levels,
    pattern = pattern,
    replacement = replacement
  )
  if (keep_levels) {
    new_levels <- union(new_levels, levels(factor_vector))
  }
  levels(factor_vector) <- new_levels
  factor_vector <- factor(factor_vector, levels = order(new_levels))
  return(factor_vector)
}
