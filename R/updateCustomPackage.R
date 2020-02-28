########################################################################################################################
#' Wrapper for easy package updating
#'
#' Wraps functions for documenting and installing packages in a short command
#'
#' @param package.location location/name of the package. Can be an indirect path
#' @export
#'
updateCustomPackage <- function(incrementing_level = "Development", package.location = getwd()) {
    incrementPackageVersion(package.location, incrementing_level = incrementing_level)
    original.warn.value <- options("warn")[[1]]
    options(warn = 1)
    devtools::document(package.location)
    devtools::build(package.location)
    devtools::install(package.location, upgrade = FALSE)
    options(warn = original.warn.value)
}
