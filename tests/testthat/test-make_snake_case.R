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

test_that("A dot anywhere is accepted", {
  expect_equal(make_snake_case(".withNum.bers123"), ".with_num.bers_123")
})

test_that("Empty spaces in the beginning don't create an underscore with a
          capital", {
            expect_equal(make_snake_case("   AbcDefGhi"), "   abc_def_ghi")
          })

test_that("Don't consider capital letters next to numbers as one word", {
  expect_equal(make_snake_case("abc123Def"), "abc_123_def")
})

test_that("Unconventional names in backticks work as expected", {
  expect_equal(make_snake_case("`123AbcDe`"), "`123_abc_de`")
})
