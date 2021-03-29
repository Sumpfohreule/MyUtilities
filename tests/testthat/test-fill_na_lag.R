testthat::test_that("Single NA returns error", {
    testthat::expect_error(fill_na_lag(NA), "Can't fill if only NAs are contained")
    testthat::expect_error(fill_na_lag(NA), "Can't fill if only NAs are contained")
})

testthat::test_that("Only NAs returns error", {
    testthat::expect_error(fill_na_lag(c(NA, NA, NA, NA)), "Can't fill if only NAs are contained")
    testthat::expect_error(fill_na_lag(c(NA, NA, NA, NA)), "Can't fill if only NAs are contained")
})

testthat::test_that("Single value in the beginning fills single NAs", {
    testthat::expect_equal(fill_na_lag(c(4, NA)), c(4, 4))
})

testthat::test_that("Single value in the beginning fills multiple NAs", {
    testthat::expect_equal(fill_na_lag(c(4, NA, NA, NA)), c(4, 4, 4, 4))
})

testthat::test_that("Last of multiple values in the beginning fills multiple NAs", {
    testthat::expect_equal(fill_na_lag(c(4, 5, 6, NA, NA, NA)), c(4, 5, 6, 6, 6, 6))
})