########################################################################################################################
#' Converts to numeric with indexed information in Debug
#'
#' Works like as.numeric, until a non-numeric value is present.
#' Then the warning/error is caught re-thrown with the indices and problematic original values printed out
#'  for better finding 'faulty' data
#'
#' @param x A value or vector which is to be converted to numeric
#' @export
#' @examples
#' as.numericTryCatch(c("2", "2.3", "A"))
#'
as.numericTryCatch <- function(x) {
  warningOrErrorCaught <- function(e) {
    already.na <- is.na(x)
    new.x <- suppressWarnings(as.numeric(x))
    new.na <- is.na(new.x)
    out.table <- data.table::data.table(
      vector.position = which(already.na == FALSE & new.na == TRUE),
      value = x[which(already.na == FALSE & new.na == TRUE)]
    )
    print(out.table)
    stop(paste0(e$message, " (See previous table)"))
  }
  tryCatch(as.numeric(x),
    error = warningOrErrorCaught,
    warning = warningOrErrorCaught
  )
}
