test_that("Fill single gap between two values", {
    expect_equal(fillGaps(c(4, 6), interval = 1), 4:6)
})

test_that("If only one value is provided, return it unchanged", {
    expect_equal(fillGaps(4, interval = 1), 4)
})

test_that("Fill single gap in reverse order", {
    expect_equal(fillGaps(c(6, 4), interval = 1), 6:4)
})

test_that("Fill gap with multiple values", {
    expect_equal(fillGaps(c(0, 10), interval = 1), 0:10)
})

test_that("Fill single gap with interval of 2", {
    expect_equal(fillGaps(c(0, 4), interval = 2), c(0, 2, 4))
})

test_that("Fill gaps within values, which first rise and then fall", {
    expect_equal(fillGaps(values = c(0, 2, 4, 2, -2), interval = 1),
                 c(0:4, 3:-2))
})

test_that("Fill gaps within values with multiple extrema", {
    expect_equal(fillGaps(values = c(6, 2, 4, 2, -2, 8, -3), interval = 1),
                 c(6:2, 3:4, 3:-2, -1:8, 7:-3))
})

test_that("Fill gaps works with dates", {
    expect_equal(fillGaps(values = as.Date(
        c("2020-01-01", "2020-01-02", "2020-01-08")),
        interval = 1),
        seq(as.Date("2020-01-01"), as.Date("2020-01-08"), 1))
})

test_that("Fill gaps works with date times", {
    expect_equal(fillGaps(values = lubridate::as_datetime(
        c("2020-01-01 00:00:00", "2020-01-01 01:00:00", "2020-01-01 12:00:00")),
        interval = 60 * 60),
        seq(lubridate::as_datetime("2020-01-01 00:00:00"),
            lubridate::as_datetime("2020-01-01 12:00:00"),
            60 * 60))
})
