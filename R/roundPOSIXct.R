########################################################################################################################
# FIXME: catch bad inputs (0, -x)
# FIXME: uneven in.seconds values can lead to unexpected results (eg. 7)
#' Round POSIXct values
#' 
#' A given POSIXct value is rounded according to the given parameters
#' 
#' @param x A POSIXct value
#' @param in.seconds A numeric value (in seconds) to round to.
#' @param round.fun A function for rounding (round, ceiling, floor, trunc)
#' @export 
#' 
roundPOSIXct <- function(x, in.seconds = calculateMainInterval(x), round.fun = round) {
    if (!"POSIXct" %in% class(x))
        stop("A POSIXct value was expected")
    time.zone <- attr(x, "tzone")
    rounded.values <- round.fun( (as.numeric(x) / in.seconds)) * in.seconds
    out.posix <- as.POSIXctFixed(rounded.values, tz = time.zone, origin = "1970-01-01")
    return(out.posix)
}
