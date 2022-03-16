test_that("calculate from single steady interval", {
  values <- c(1, 3, 5, 7)
  expect_equal(calculateMainInterval(values), 2)
})

test_that("calculate interval main interval with one frequent and one less frequent interval", {
  values <- c(1, 3, 5, 7, 100, 200, 300, 400, 500, 600, 700)
  expect_equal(calculateMainInterval(values), 100)
})

test_that("Throw error if only a single value is provided as there can't be an interval", {
  expect_error(calculateMainInterval(100))
})

test_that("Throw error if no value is provided as there can't be an interval", {
  expect_error(calculateMainInterval())
})

test_that("Throw error if multiple intervals have the same frequency", {
  expect_error(calculateMainInterval(c(1, 2, 3, 10, 20, 30, 100, 200, 300)))
})
