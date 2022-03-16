#' Imports FVA specific Level2 aggregated xlsx-file (Gesamt)
#'
#' @param xlsx_path A file path (or a workbook) to an excel with a specific
#' structure (Two header columns; date column named 'Datum')
#' @param sheet Single sheet name of the excel file to be imported
#' @return A data.table
#' @export
#'
importAggregateExcelSheet <- function(xlsx_path, sheet) {
  if (assertthat::is.string(xlsx_path)) {
    assertthat::assert_that(is_contained(
      sheet,
      openxlsx::getSheetNames(xlsx_path)
    ))
  } else {
    assertthat::assert_that(is_contained(sheet, names(xlsx_path)))
  }
  input_sheet <- openxlsx::read.xlsx(
    xlsx_path,
    sheet = sheet
  )
  names(input_sheet) <- stringr::str_replace(names(input_sheet),
    pattern = "^.*Datum.*$",
    replacement = "Datum"
  )
  original_names <- names(input_sheet)

  date_col <- sym("Datum")
  output_sheet <- input_sheet %>%
    as_tibble(.name_repair = "unique") %>%
    slice(-1) %>%
    purrr::map(function(x) {
      col_type <- readr::guess_parser(x)
      conversion_function <- get(paste0("as.", col_type))
      return(conversion_function(x))
    }) %>%
    as_tibble() %>%
    mutate(!!date_col := as.POSIXct(!!date_col * 60 * 60 * 24,
      origin = "1899-12-30",
      tz = "UTC"
    )) %>%
    mutate(!!date_col := lubridate::round_date(!!date_col, "1min")) %>%
    data.table::as.data.table()
  names(output_sheet) <- original_names
  return(output_sheet)
}
