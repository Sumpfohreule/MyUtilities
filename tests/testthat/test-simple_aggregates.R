# min_with_default
test_that("without NAs min_with_default equals min", {
    expect_equal(min_with_default(10), min(10))
    expect_equal(min_with_default(c(0, 10)), min(c(0, 10)))
    expect_equal(min_with_default(c(5, 5)), min(c(5, 5)))
    expect_equal(min_with_default(c(10, -10)), min(c(10, -10)))
})

test_that("min_with_default does not consider NAs, if values are present", {
    expect_equal(min_with_default(c(NA, 10)), 10)
    expect_equal(min_with_default(c(10, NA)), 10)
})

test_that("min_with_default returns given default, if only NAs are present", {
    expect_equal(min_with_default(NA), 0)
    expect_equal(min_with_default(NA, default = 100), 100)
    expect_equal(min_with_default(NA, default = -111), -111)
})

# max_with_default
test_that("without NAs max_with_default equals max", {
    expect_equal(max_with_default(10), max(10))
    expect_equal(max_with_default(c(0, 10)), max(c(0, 10)))
    expect_equal(max_with_default(c(5, 5)), max(c(5, 5)))
    expect_equal(max_with_default(c(10, -10)), max(c(10, -10)))
})

test_that("max_with_default does not consider NAs, if values are present", {
    expect_equal(max_with_default(c(NA, 10)), 10)
    expect_equal(max_with_default(c(10, NA)), 10)
})

test_that("max_with_default returns given default, if only NAs are present", {
    expect_equal(max_with_default(NA), 0)
    expect_equal(max_with_default(NA, default = 100), 100)
    expect_equal(max_with_default(NA, default = -111), -111)
})

# mean_with_default
test_that("without NAs mean_with_default equals mean", {
    expect_equal(mean_with_default(10), mean(10))
    expect_equal(mean_with_default(c(0, 10)), mean(c(0, 10)))
    expect_equal(mean_with_default(c(5, 5)), mean(c(5, 5)))
    expect_equal(mean_with_default(c(10, -10)), mean(c(10, -10)))
})

test_that("mean_with_default does not consider NAs, if values are present", {
    expect_equal(mean_with_default(c(NA, 10)), 10)
    expect_equal(mean_with_default(c(10, NA)), 10)
})

test_that("mean_with_default returns given default, if only NAs are present", {
    expect_equal(mean_with_default(NA), 0)
    expect_equal(mean_with_default(NA, default = 100), 100)
    expect_equal(mean_with_default(NA, default = -111), -111)
})

# sum_with_default
test_that("without NAs sum_with_default equals sum", {
    expect_equal(sum_with_default(10), sum(10))
    expect_equal(sum_with_default(c(0, 10)), sum(c(0, 10)))
    expect_equal(sum_with_default(c(5, 5)), sum(c(5, 5)))
    expect_equal(sum_with_default(c(10, -10)), sum(c(10, -10)))
})

test_that("sum_with_default does not consider NAs, if values are present", {
    expect_equal(sum_with_default(c(NA, 10)), 10)
    expect_equal(sum_with_default(c(10, NA)), 10)
})

test_that("sum_with_default returns given default, if only NAs are present", {
    expect_equal(sum_with_default(NA), 0)
    expect_equal(sum_with_default(NA, default = 100), 100)
    expect_equal(sum_with_default(NA, default = -111), -111)
})