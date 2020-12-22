test_that("Single value is remaped with only the new level", {
    testthat::expect_equal(remapLevels(factor("a"), pattern = "a", replacement = "b", keep_levels = FALSE), factor("b"))
})

test_that("Single value is remaped with both levels kept", {
    expected <- factor("b")
    levels(expected) <- c("a", "b")
    testthat::expect_equal(remapLevels(factor("a"), pattern = "a", replacement = "b", keep_levels = TRUE), expected )
})