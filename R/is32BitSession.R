########################################################################################################################
#' Tests if current R-Session is 32-bit
#' 
#' @export 
#' 
is32BitSession <- function() {
    r.bit.version <- sessionInfo()[["platform"]]
    is32Bit <- grepl("^.+\\(32-bit\\)", r.bit.version, perl = TRUE)
    return(is32Bit)
}
