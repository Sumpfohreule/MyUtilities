########################################################################################################################
#' Fix for as.POSXct not accepting tz
#' 
#' This small fix allows usage of the timezone argument for Date objects.
#' 
#' @param x A Date value to be converted to POSIXct
#' @param ... Optional values including tz for specifying time zone
#' @export 
#' 
as.POSIXctFixed <- function(x, ...) {
    if ("Date" %in% class(x)) {
        return(.POSIXct(unclass(x) * 86400, ...))
    } else {
        return(as.POSIXct(x, ...))
    }
}
