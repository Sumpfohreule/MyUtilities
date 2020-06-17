testthat::test_that("Single element is contained in larger set", {
    testthat::expect_true(is_contained("abc", c("a", "d", "ef", "abc")))
    testthat::expect_true(is_contained(5, 0:10))
    testthat::expect_true(is_contained(2.0123, c(0, 2.0123, .3)))
})

testthat::test_that("Single element is not contained in larger set", {
    testthat::expect_false(is_contained("abc", c("a", "d", "ef", "dabc")))
    testthat::expect_false(is_contained(11, 0:10))
    testthat::expect_false(is_contained(2.0123, c(0, 2.012, .3)))
})

testthat::test_that("Sets of same size", {
    testthat::expect_true(is_contained(c("abc", "a", "ef", "d"), c("a", "d", "ef", "abc")))
    testthat::expect_true(is_contained(0:10, 10:0))
    testthat::expect_true(is_contained(c(0, 2.012, .3), c(2.012, .3, 0)))
})

testthat::test_that("FALSE on empty container_set", {
    testthat::expect_false(is_contained(0, NULL))
})

testthat::test_that("NULL for test_set always works", {
    testthat::expect_true(is_contained(NULL, "abc"))
    testthat::expect_true(is_contained(NULL, 0))
    testthat::expect_true(is_contained(NULL, c(1,2,3)))
})

testthat::test_that("Numbers can be tested with strings", {
    testthat::expect_true(is_contained(123, "123"))
    testthat::expect_true(is_contained(123.3, "123.3"))
    testthat::expect_true(is_contained("123", 123))
    testthat::expect_true(is_contained("123.3", 123.3))

    testthat::expect_false(is_contained(123, c("13", "abc")))
    testthat::expect_false(is_contained(c("123"), c(1, 2, 3)))
})

testthat::test_that("FALSE for larger test_set than container_set", {
    testthat::expect_false(is_contained(c(123, 124), 123))
})