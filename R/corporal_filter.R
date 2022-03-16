#' Wrapper for dplyr filter which can pass NULL values through
#'
#' @seealso \link{dplyr::filter}
#' @export
#'
corporal_filter <- function(.data, ..., .preserve = FALSE) {
  if (!is.null(.data)) {
    return(filter(.data, ..., .preserve = .preserve))
  } else {
    return(NULL)
  }
}
