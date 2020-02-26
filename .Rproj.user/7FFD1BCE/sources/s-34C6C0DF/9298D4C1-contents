########################################################################################################################
#' Returns path for the most recent modfied file
#'
#' Within a given folder path the path to the file with the most recent changes is returned
#'
#' @param folder Path to the folder, containing the files to select from
#' @param pattern A regex pattern to select files by
#' @param recursive Flag to determine if folders should be included recursively
#' @return Returns path of a single file which was modified most recently. Error is thrown if none was found!
#' @export
#'
getLastModifiedFile <- function(folder, pattern = NULL, recursive = FALSE) {
    latest_file <- data.frame(path = dir(folder, full.names = TRUE, pattern = pattern, recursive = recursive)) %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::filter(!dir.exists(path)) %>% # Filter foldes
        dplyr::filter(!stringr::str_detect(basename(path), pattern = "^~\\$")) %>% # Filter excel lock files
        dplyr::filter(file.mtime(path) == max(file.mtime(path))) %>%
        dplyr::pull(path)
    if (file.exists(latest_file)) {
        return(latest_file)

    } else {
        stop("No files have been found with the given pattern '", pattern, "' within folder\n", folder)
    }
}
