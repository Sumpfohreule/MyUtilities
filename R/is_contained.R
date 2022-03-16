#' Tests if \code{test_set} is contained in \code{container_set}
#'
#' Simple test function which has a custom error message for assertthat
#' @export
is_contained <- function(test_set, container_set) {
  return(!(FALSE %in% (test_set %in% container_set)))
}

#' @export
assertthat::on_failure(is_contained) <- function(call, env) {
  paste0("Values from '", deparse(call$test_set), "' are not contained in '", deparse(call$container_set), "'")
}
