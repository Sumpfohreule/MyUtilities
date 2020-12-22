test_that("Single value is remaped with only the new level", {
    testthat::expect_equal(remapLevels(factor("a"), pattern = "a", replacement = "b", keep_levels = FALSE), factor("b"))
})

test_that("Single value is remaped with both levels kept", {
    expected <- factor("b")
    levels(expected) <- c("b", "a")
    testthat::expect_equal(remapLevels(factor("a"), pattern = "a", replacement = "b", keep_levels = TRUE), expected )
})

test_that("Remapping of a value from multiple values with dropping the old levels", {
    expected <- factor(c("a", "xxx", "c"), levels = c("a", "xxx", "c"))
    testthat::expect_equal(remapLevels(c("a", "b", "c"), pattern = "b", replacement = "xxx", keep_levels = FALSE), expected)
})

test_that("Remapping of a value from multiple values with dropping the old levels", {
    expected <- factor(c("a", "xxx", "c"), levels = c("a", "xxx", "c", "b"))
    testthat::expect_equal(remapLevels(c("a", "b", "c"), pattern = "b", replacement = "xxx", keep_levels = TRUE), expected)
})
