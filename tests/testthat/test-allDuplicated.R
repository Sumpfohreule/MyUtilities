test_that("Two value vector with duplicates returns TRUE for both", {
    expect_equal(allDuplicated(c(2, 2)), c(TRUE, TRUE))
})

test_that("Two value vector without duplicates returns FALSE for both", {
    expect_equal(allDuplicated(c(10, 2)), c(FALSE, FALSE))
})

test_that("Works with filtering if a single column is selected", {
    test_df <- data.frame(vars = c("a", "a", "b", "b", "c"), values = c(1:5))
    expect_equal(
        object = filter(test_df, allDuplicated(vars)),
        expected = test_df[-5, ])
})

