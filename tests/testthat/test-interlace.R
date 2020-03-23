test_that("Interlacing begins with the left vector", {
    vec_1 <- c(22, 33)
    vec_2 <- c(1, 5)

    expect_equal(interlace(vec_1, vec_2), c(22, 1, 33, 5))
})

test_that("First vector is shorter", {
    vec_1 <- c(1, 5)
    vec_2 <- c(4, 6, 8)

    expect_equal(interlace(vec_1, vec_2), c(1, 4, 5, 6, 8))
})

test_that("First vector is shorter multiple times", {
    vec_1 <- c(1, 5)
    vec_2 <- c(4, 6, 8, 10, 20, 30, 40, 50, 60)

    expect_equal(interlace(vec_1, vec_2), c(1, 4, 5, 6, 8, 10, 20, 30, 40, 50, 60))
})