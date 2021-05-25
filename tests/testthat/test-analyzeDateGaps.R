test_that("Return no gap if only one date is provided", {
    empty_output <- data.frame(gap_start = lubridate::Date(),
                               gap_end = lubridate::Date(),
                               gap_size_percent = numeric())
    expect_equal(analyzeDateGaps(lubridate::ymd("2020-01-01")), empty_output)
})


test_that("Works with dplyr::group_map", {
    calculated <- data.frame(groups = c("a", "a", "b", "b"),
                             dates = lubridate::ymd_hms(c("2020-01-01 00:00:00",
                                                          "2020-01-01 02:00:00",
                                                          "2020-01-01 00:00:00",
                                                          "2020-01-01 05:00:00"))) %>%
        group_by(groups) %>%
        group_map(~ .x %>% pull(dates) %>% analyzeDateGaps(interval = 60 * 60))
    reference <- list(
        data.frame(gap_start = lubridate::ymd_hms("2020-01-01 00:00:00"),
                   gap_end = lubridate::ymd_hms("2020-01-01 02:00:00"),
                   gap_size_percent = 33),
        data.frame(gap_start = lubridate::ymd_hms("2020-01-01 00:00:00"),
                   gap_end = lubridate::ymd_hms("2020-01-01 05:00:00"),
                   gap_size_percent = 67)
    )
    expect_equal(calculated, reference)
})

test_that("Analyze gaps between latest given date and the end of the year", {
    dates <- lubridate::ymd(c("2020-01-01",
                              "2020-01-02",
                              "2020-01-03"))
    reference <- data.frame(gap_start = lubridate::as_datetime("2020-01-03"),
                            gap_end = lubridate::as_datetime("2020-12-31"),
                            gap_size_percent = 99)
    expect_equal(analyzeDateGaps(dates, interval = 60 * 60 * 24, extend.to.full.year = TRUE),
                 reference)
})

test_that("Analyze gaps between latest given date and the start of the year", {
    dates <- lubridate::ymd(c("2020-12-29",
                              "2020-12-30",
                              "2020-12-31"))
    reference <- data.frame(gap_start = lubridate::as_datetime("2020-01-01"),
                            gap_end = lubridate::as_datetime("2020-12-29"),
                            gap_size_percent = 99)
    expect_equal(analyzeDateGaps(dates, interval = 60 * 60 * 24, extend.to.full.year = TRUE),
                 reference)
})
