testthat::test_that("Single NA value is returned as is", {
    testthat::expect_equal(fill_na_lag(NA), NA)
})

testthat::test_that("Only NAs are returned as is", {
    testthat::expect_equal(fill_na_lag(c(NA, NA, NA, NA)), c(NA, NA, NA, NA))
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

testthat::test_that("NA value without any previous values is not filled", {
    testthat::expect_equal(fill_na_lag(c(NA, NA, 3, NA)), c(NA, NA, 3, 3))
})
