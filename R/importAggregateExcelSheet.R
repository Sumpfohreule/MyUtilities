#' Imports FVA specific Level2 aggregated xlsx-file (Gesamt)
#'
#' @param xlsx_path A file path to an excel with a specific structure (Two header columns; date column named 'Datum')
#' @param sheet Single sheet name of the excel file to be imported
#' @return A data.table
#' @export
#'
importAggregateExcelSheet <- function(xlsx_path, sheet) {
    tryCatch({
        xlsx_sheet <- openxlsx::read.xlsx(
            xlsx_path,
            sheet = sheet) %>%
            slice(-1) %>%
            purrr::map(function(x) {
                col_type <- readr::guess_parser(x)
                conversion_function <- get(paste0("as.", col_type))
                return(conversion_function(x))
            }) %>%
            as.data.frame() %>%
            mutate(Datum = as.POSIXct(
                Datum * 60 * 60 * 24,
                origin = "1899-12-30",
                tz = "UTC")) %>%
            mutate(Datum = roundPOSIXct(
                Datum,
                in.seconds = 5 * 60,
                round.fun = round)) %>%
            data.table::as.data.table()
    }, error = function(e) {
        message <- geterrmessage()
        if (!stringr::str_detect(message, "Cannot find sheet named"))
            stop(e)
    })
    return(xlsx_sheet)
}
