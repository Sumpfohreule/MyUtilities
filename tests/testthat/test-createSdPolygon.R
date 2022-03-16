test_that("Create a simple SD-Polygon", {
  expect_equal(
    createSdPolygon(x = 1:3, y = c(1, 1, 1), sd = 1),
    data.frame(x = c(1:3, 3:1), y = c(0, 0, 0, 2, 2, 2))
  )
})

test_that("Create SD-Polygon around rising data", {
  expect_equal(
    createSdPolygon(x = 1:3, y = 11:13, sd = 1),
    data.frame(x = c(1:3, 3:1), y = c(10, 11, 12, 14, 13, 12))
  )
})

test_that("Error if x and y values in createSdPolygon have different lengths", {
  expect_error(createSdPolygon(x = 1:3, y = 1:10, sd = 1))
  expect_error(createSdPolygon(x = 1:10, y = 1:3, sd = 1))
})
