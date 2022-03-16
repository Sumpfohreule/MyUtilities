#' Determine duplicate elements including its first occurrence
#'
#' allDuplicated works similar to duplicated, but also returns true for the
#' first occurrence
#' of any duplicate. The function only works on vectors or single columns of a
#' data.frame for now
#'
#' @param x A vector of values
#' @return A boolean vector
#' @examples
#' # Usage with a vector
#' expect_equal(
#'   object = allDuplicated(c(1, 1, 3, 5, 5, 1)),
#'   expected = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
#' )
#'
#' # Filtering a data.frame
#' test_df <- data.frame(vars = c("a", "a", "b", "b", "c"), values = c(1:5))
#' expect_equal(
#'   object = filter(test_df, allDuplicated(vars)),
#'   expected = test_df[-5, ]
#' )
#' @export
#'
allDuplicated <- function(x) {
  assertthat::assert_that(is.vector(x))
  simple_duplicates <- duplicated(x)
  duplicated_values <- x[simple_duplicates]
  if (length(duplicated_values) > 0) {
    return(x %in% duplicated_values)
  } else {
    return(rep(FALSE, length(x)))
  }
}
