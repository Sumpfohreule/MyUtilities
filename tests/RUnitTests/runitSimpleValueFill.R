# TODO: Convert tests to testthat!
test_that("testNothingoReplace", {
  original_table <- data.frame(x = 10:1, y = 21:30)

  filled_table <- simpleValueFill(original_table, c("x", "y"))

  expect_equal(original_table, filled_table)
})

test_that("testSingleReplacementWithoutShift", {
  original_table <- data.frame(x = 1:10, y = c(1:4, NA, 6:10))
  target_table <- data.frame(x = 1:10, y = c(1:4, 5, 6:10))

  filled_table <- simpleValueFill(original_table, c("x", "y"))

  expect_equal(target_table, filled_table)
})

test_that("testSingleReplacementWithShiftOf_5", {
  original_table <- data.frame(x = 1:10, y = c(6:10, NA, 12:15))
  target_table <- data.frame(x = 1:10, y = c(6:10, 11, 12:15))

  filled_table <- simpleValueFill(original_table, c("x", "y"))

  expect_equal(target_table, filled_table)
})

test_that("testFillInFromAndToMultipleVectors", {
  original_table <- data.frame(
    x = c(NA, NA, 3:7, rep(NA, 5)),
    y = c(1:2, rep(NA, 2), 5:8, rep(NA, 2), 11:12),
    z = c(rep(NA, 5), 6:10, NA, NA)
  )
  target_table <- data.frame(x = 1:12, y = 1:12, z = 1:12)

  filled_table <- simpleValueFill(original_table, c("x", "y", "z"))

  expect_equal(target_table, filled_table)
})

test_that("testSimpleFillIfNoOverlap", {
  original_table <- data.frame(
    x = c(1:5, rep(NA, 5)),
    y = c(rep(NA, 5), 10:6)
  )
  target_table <- data.frame(
    x = c(1:5, 10:6),
    y = c(1:5, 10:6)
  )

  filled_table <- simpleValueFill(original_table, value_cols = c("x", "y"))
  expect_equal(target_table, filled_table)
})

test_that("testUseMeanIfNoOverlapAndMultipleColumns", {
  original_table <- data.frame(
    x = c(rep(NA, 15)),
    y = c(1:15),
    z = c(seq(3, 45, 3))
  )
  target_table <- data.frame(
    x = seq(2, 30, 2),
    y = c(1:15),
    z = seq(3, 45, 3)
  )

  filled_table <- simpleValueFill(original_table, value_cols = c("x", "y", "z"))
  expect_equal(target_table, filled_table)
})

test_that("testErrorForOnlySingleColumn", {
  original_table <- data.frame(x = 1:15)

  expect_error(simpleValueFill(original_table, "x"))
})

test_that("testErrorForOnlySingleColumnSelected", {
  original_table <- data.frame(x = 1:15, y = 1:15)

  expect_error(simpleValueFill(original_table, "x"))
})

test_that("testErrorForTableColumnMismatch", {
  original_table <- data.frame(x = 1:15, y = 1:15, z = 1:15)

  expect_error(simpleValueFill(
    original_table,
    c("x", "x", "DoesNotExist")
  ))
})

test_that("testUseOnlySomeColumns", {
  original_table <- data.frame(x = 1:15, y = rep(NA, 15), z = seq(2, 30, 2))
  only_use_xy <- c("x", "y")
  target_table <- data.frame(x = 1:15, y = 1:15, z = seq(2, 30, 2))

  filled_table <- simpleValueFill(original_table, only_use_xy)
  expect_error(target_table, filled_table)
})

test_that("testErrorForDuplicatedColumnNames", {
  original_table <- data.frame(x = 1:15, y = 1:15)
  names(original_table) <- c("x", "x")
  expect_error(simpleValueFill(original_table, c("x", "x")))
})

test_that("testFillWithNonStandardColumnNames", {
  original_table <- data.frame(x = 1:15, y = 1:15)
  names(original_table) <- c("001", "002")

  filled_table <- simpleValueFill(original_table, value_cols = c("001", "002"))

  expect_equals(target = c("001", "002"), names(filled_table))
})

test_that("testAllValuesAreEmpty", {
  original_table <- data.frame(
    x = as.numeric(rep(NA, 10)),
    y = as.numeric(rep(NA, 10))
  )

  filled_table <- simpleValueFill(original_table, c("x", "y"))

  expect_equals(original_table, filled_table)
})

test_that("testOnMoreThanTwoColumnsWithOnlyNaNoNanIsReturned", {
  value_table <- data.frame(
    x = c(1:5, NA, 7:10), y = c(1:5, NA, 7:10),
    z = c(1:5, NA, 7:10)
  )

  filled_table <- simpleValueFill(value_table, value_cols = c("x", "y", "z"))

  # check first if values are NA or NaN (is.na) and afterwards that non are NaN
  # (NA or NaN) and not NaN = NA and not NaN
  expect_equals(TRUE %in% is.na(unlist(filled_table[6, ])))
  expect_equals(!(TRUE %in% is.nan(unlist(filled_table[6, ]))))
})

test_that("testErrorOnOnlyOneColumn", {
  value_table <- data.frame(x = 1:10)
  expect_equals(simpleValueFill(value_table, value_cols = c("x")))
})

test_that("testErrorOnProvidedColsNotExisting", {
  value_table <- data.frame(
    x = c(1:5, NA, 7:10), y = c(1:5, NA, 7:10),
    z = c(1:5, NA, 7:10)
  )
  expect_equals(simpleValueFill(value_table,
    value_cols = c("x", "not_there")
  ))
})
