test_that("String that is already snake case", {
  expect_equal(make_snake_case("is_snake_case"), "is_snake_case")
})

test_that("Change single camelCase to snake_case", {
  expect_equal(make_snake_case("notSnake"), "not_snake")
})

test_that("Change multiple camelCases to snake_case", {
  expect_equal(make_snake_case("thisIsCamelCase"), "this_is_camel_case")
})

test_that("No underscore at the beginning of a string", {
  expect_equal(make_snake_case("NotReallyCamelCase"), "not_really_camel_case")
})

test_that("Make multiple adjacent capital letters lower case with only one
          underscore", {
  expect_equal(make_snake_case("acronymCCD"), "acronym_ccd")
})

test_that("Also add underscore bevor numbers", {
  expect_equal(make_snake_case("withNumbers123"), "with_numbers_123")
})

test_that("Apply function to string vector", {
  expect_equal(
    object = make_snake_case(
      c(
        "camelCase1",
        "camelCase2",
        "camelCase3"
      )
    ),
    expected = c(
      "camel_case_1",
      "camel_case_2",
      "camel_case_3"
    )
  )
})

test_that("Only accept strings which can be variable/function names", {
  # Starting with a character, followed by characters or numbers
  expect_error(make_snake_case("  aBdeCdd"))
  expect_error(make_snake_case("123NotCorrect"))
  expect_error(make_snake_case("NotCorrect   "))
  expect_error(make_snake_case("NoOtherSymbols!Error"))
})
