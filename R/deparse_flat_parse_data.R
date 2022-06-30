deparse_flat_parse_data <- function(pd_table) {
    # browser()
    # Solution from https://stackoverflow.com/questions/44786316/reverse-of-getparsedata-from-parsed-code-back-to-code
    # by user mnist
    nz_pd_table <- pd_table %>%
        filter(nzchar(text))
    deparsed <- by(data = nz_pd_table,
                   # consider each line separately
                   INDICES = list(nz_pd_table$line1),
                   FUN = function(args) {
                       # browser()
                       # determine number of tokens in the line
                       n_tokens <- length(args$text)

                       # determine width of white space needed prior to each
                       # token it is its starting position minus
                       # the end position of the precessing token minus 1
                       # since positions are including
                       n_ws <- args$col1 - c(0, args$col2[-n_tokens]) - 1

                       print(args$text)

                       # create needed whitespace
                       ws <- strrep(" ", n_ws)

                       # add whitespaces to the tokens
                       paste(ws, args$text, collapse = "", sep = "")
                   })
    return(deparsed)
}
