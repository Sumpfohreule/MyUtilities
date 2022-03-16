########################################################################################################################
#' Calculates the binomial coefficient
#'
#' The "simplified" function for binomial calculation (Not working for Real Numbers)
#'
#' @param n Number of all elements of a set
#' @param k Length of the possible combinations to be selected
#' @export
#' @examples
#' # Number of possible combinations by selecting 6 elements out of 49 (Lotto)
#' binomial(49, 6)
#'
binomialCoefficient <- function(n, k) {
  factorial(n) / (factorial(k) * factorial(n - k))
}
