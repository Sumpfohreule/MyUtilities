.clear_tempdir <- function() {
    temp_dir_files <- dir(tempdir(), full.names = TRUE)
    unlink(temp_dir_files, recursive = TRUE)
}

.createDummyDataFrame <- function(column_names) {
    dummy_data_count <- length(column_names) * 5
    dummy_data <- matrix(1:dummy_data_count, ncol = length(column_names)) %>%
        data.frame() %>%
        data.table::setnames(column_names)

    # Should be character but can't have mixed units
    fake_numeric_unit_row <- rep(9999, length(column_names))
    dummy_data <- rbind(fake_numeric_unit_row, dummy_data)
    return(dummy_data)
}


test_that("Empty columns with header are imported correctly", {
    .clear_tempdir()
    test_data <- .createDummyDataFrame(c("Datum", "Temp_900", "Temp_904", "SE_900", "SE_904"))
    test_data[-1, "SE_900"] <- NA
    test_data[-1, "SE_904"] <- NA
    path <- file.path(tempdir(), "Test.xlsx")
    openxlsx::write.xlsx(test_data, path, sheetName = "Fichte")


    expect_equal(object = names(importAggregateExcelSheet(xlsx_path = path, sheet = "Fichte")),
                 expected = c("Datum", "Temp_900", "Temp_904", "SE_900", "SE_904"))
})


test_that("Duplicated columns are handled without error", {
    .clear_tempdir()
    test_data <- .createDummyDataFrame(c("Datum", "PR", "PR", "Temp_900", "Temp_904", "SE_900", "SE_904"))
    path <- file.path(tempdir(), "Test.xlsx")
    openxlsx::write.xlsx(test_data, path, sheetName = "Fichte")

    modified_table <- importAggregateExcelSheet(path, sheet = "Fichte")
    expect_equal(names(modified_table), c("Datum", "PR", "PR", "Temp_900", "Temp_904", "SE_900", "SE_904"))
})
