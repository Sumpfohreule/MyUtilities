test_that("multiplication works", {
    dir("R", full.names = TRUE) %>%
        purrr::map(readLines) %>%
        purrr::map(~ {
            browser()
            parsed_deparsed <- parse(text = .x) %>%
                getParseData() %>%
                deparse_flat_parse_data()
        })
})
