########################################################################################################################
# TODO: Maybe remove date.col as it probably is better done externaly
#' Query data from an Access-DB (32 Bit)
#'
#' A SQL-Query for fetching data from an access db is passed,
#' executed within a separate 32-Bit R session and saved to disk.
#'
#' @param db.path Path the the access db file
#' @param sql.query The Query for fetching the data
#' @param date.col Name the date column
#' @param out.file File path for exporting the data to
#' @export
#'
access32BitQuery <- function(db.path, sql.query, date.col, out.file) {
    suppressWarnings(library(RODBC))
    suppressWarnings(library(data.table))

    # variables to make values uniform
    ODBC_con <- "a32_con"
    data <- "temp.table"

    if (file.exists(db.path)) {
        # build expression to pass to 32 bit R session
        expr <- "suppressWarnings(library(svSocket))"
        expr <- c(expr, "suppressWarnings(library(RODBC))")
        expr <- c(expr, "suppressWarnings(library(data.table))")
        expr <- c(expr, "options(warn=1)")
        expr <- c(expr, sprintf("%s <- odbcConnectAccess('%s')", ODBC_con, db.path))
        expr <- c(expr, sprintf("%s <- as.data.table(sqlQuery(%s, %s, as.is = TRUE))", data, ODBC_con, sql.query))
        expr <- c(expr, sprintf("%1$s[, %2$s := as.POSIXct(%2$s, tz = 'UTC')]", data, date.col))
        expr <- c(expr, sprintf("setkey(%1$s, %2$s)", data, date.col))
        expr <- c(expr, sprintf("close(%s)", ODBC_con))
        expr <- c(expr, sprintf("saveRDS(%s, '%s')", data, out.file))
        expr <- paste(expr, collapse = ";")

        # launch 32 bit R session and run expressions
        prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
        system2(prog, args = c("-e", shQuote(expr)), stdout = NULL, wait = TRUE, invisible = TRUE)

        # display table fields
    } else {
        stop("database not found: ", db.path)
    }
}
