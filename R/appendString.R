########################################################################################################################
#' Appending a string with separator if needed
#' 
#' Appending string to a source and seperating both with a separator.
#' If the source is empty the separator is ignored.
#' 
#' @param source.string The source value to which to append to. If it is NA or empty sep will be ignored.
#' @param append.string The string which is to be appended to the source. If source is empty only this string is returned
#' @param sep The seperator which is pasted between both strings
#' @export 
#' @examples 
#' appendString("", "Hello")
#' # "Hello
#' 
#' appendString(NA, "Hello")
#' # "Hello"
#' 
#' appendString("Hi", "Hello")
#' # "Hi; Hello"
#' 
appendString <- function(source.string, append.string, sep = "; ") {
    if (is.na(source.string) | nchar(source.string) == 0) {
        return(append.string)
    } else {
        return(paste(source.string, append.string, sep = sep))
    }
}
