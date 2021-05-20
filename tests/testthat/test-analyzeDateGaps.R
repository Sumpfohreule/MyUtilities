test_that("analyzeDateGaps works with dplyr::group_map", {
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
