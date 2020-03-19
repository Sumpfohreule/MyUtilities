#' Converts encoding of a plain text file to another encoding
#'
#' @param path path to the file
#' @param from current encoding of the given file
#' @param to target encoding
#' @details The provided file will not be overritten. A new file will be created with the target encoding appended at the end
#' @export
convertFileEncoding <- function(path, from = "ISO-8859-14", to = "UTF-8") {
    writeLines(iconv(readLines(path), from = from, to = to),
               file(paste0(path, "_", to), encoding = to))
}