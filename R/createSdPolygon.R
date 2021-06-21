########################################################################################################################
# TODO: check if sd-value can just be calculated within this function!
#' Creating a sd polygon-table
#'
#' This function creates a table, which can be used to plot a sd-polygon in ggplot etc.
#' @param x A numeric vector with the same length as y
#' @param y A numeric vector with the same length as x
#' @param sd The standard deviation value calculated for the data
#' @export
#'
createSdPolygon <- function(x, y, sd) {
    assertthat::assert_that(length(x) == length(y))
    sd.top <- y + sd
    sd.bot <- y - sd
    data.frame(x = c(x, rev(x)), y = c(sd.bot, rev(sd.top)))
}
