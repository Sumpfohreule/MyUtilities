test_that("Keeping single element of a simple list", {
    simple_list <- list(a = 2, b = 3, c = 4)
    expect_equal(keep_names(simple_list, ~ .x == "b"), list(b = 3))
})

test_that("Keeping multiple elements with the same name of a simple list", {
    simple_list <- list(a = 2, b = 3, c = 4, d = 5, c = 6)
    expect_equal(keep_names(simple_list, ~ .x == "c"), list(c = 4, c = 6))
})

test_that("Using %in% to look for multiple names in a simple list", {
    simple_list <- list(a = 2, b = 3, c = 4, d = 5)
    expect_equal(keep_names(simple_list, .p = ~ .x %in% c("a", "d")), list(a = 2, d = 5))
})

test_that("Find part names with stringr::str_detect", {
    simple_list <- list(X_FDR_Y = 2, Y_MP_Z3 = 3, A_FDR_33 = 4, D = 5)
    expect_equal(keep_names(simple_list, .p = ~ stringr::str_detect(.x, "_FDR_")),
                 list(X_FDR_Y = 2, A_FDR_33 = 4))
})

test_that("Find part names with stringr::str_detect, without using a formula", {
    simple_list <- list(X_FDR_Y = 2, Y_MP_Z3 = 3, A_FDR_33 = 4, D = 5)
    expect_equal(keep_names(simple_list, stringr::str_detect, pattern = "_FDR_"),
                 list(X_FDR_Y = 2, A_FDR_33 = 4))
})