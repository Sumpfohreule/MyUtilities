test_that("Create a simple SD-Polygon", {
    expect_equal(createSdPolygon(x = 1:3, y = c(1, 1, 1), sd = 1),
                 data.frame(x = c(1:3, 3:1), y = c(0, 0, 0, 2, 2, 2)))
})
