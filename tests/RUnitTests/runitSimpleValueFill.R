########################################################################################################################
testNothingToReplace <- function() {
    original_table <- data.frame(x = 10:1, y = 21:30)

    filled_table <- simpleValueFill(original_table, c("x", "y"))

    RUnit::checkEquals(original_table, filled_table)
}

testSingleReplacementWithoutShift <- function() {
    original_table <- data.frame(x = 1:10, y = c(1:4, NA, 6:10))
    target_table <- data.frame(x = 1:10, y = c(1:4, 5, 6:10))

    filled_table <- simpleValueFill(original_table, c("x", "y"))

    RUnit::checkEquals(target_table, filled_table)
}

testSingleReplacementWithShiftOf_5 <- function() {
    original_table <- data.frame(x = 1:10, y = c(6:10, NA, 12:15))
    target_table <- data.frame(x = 1:10, y = c(6:10, 11, 12:15))

    filled_table <- simpleValueFill(original_table, c("x", "y"))

    RUnit::checkEquals(target_table, filled_table)
}

testFillInFromAndToMultipleVectors <- function() {
    original_table <- data.frame(
        x = c(NA, NA, 3:7, rep(NA, 5)),
        y = c(1:2, rep(NA, 2), 5:8, rep(NA, 2), 11:12),
        z = c(rep(NA, 5), 6:10, NA, NA))
    target_table <- data.frame(x = 1:12, y = 1:12, z = 1:12)

    filled_table <- simpleValueFill(original_table, c("x", "y", "z"))

    RUnit::checkEquals(target_table, filled_table)
}

testSimpleFillIfNoOverlap <- function() {
    original_table <- data.frame(
        x = c(1:5, rep(NA, 5)),
        y = c(rep(NA, 5), 10:6))
    target_table <- data.frame(
        x = c(1:5, 10:6),
        y = c(1:5, 10:6))

    filled_table <- simpleValueFill(original_table, value_cols = c("x", "y"))
	RUnit::checkEquals(target_table, filled_table)
}

testUseMeanIfNoOverlapAndMultipleColumns <- function() {
    original_table <- data.frame(
        x = c(rep(NA, 15)),
        y = c(1:15),
        z = c(seq(3, 45, 3)))
    target_table <- data.frame(
        x = seq(2, 30, 2),
        y = c(1:15),
        z = seq(3, 45, 3))

    filled_table <- simpleValueFill(original_table, value_cols = c("x", "y", "z"))
    RUnit::checkEquals(target_table, filled_table)
}

testErrorForOnlySingleColumn <- function() {
    original_table <- data.frame(x = 1:15)

    RUnit::checkException(simpleValueFill(original_table, "x"))
}

testErrorForOnlySingleColumnSelected <- function() {
    original_table <- data.frame(x = 1:15, y = 1:15)

    RUnit::checkException(simpleValueFill(original_table, "x"))
}

testErrorForTableColumnMismatch <- function() {
    original_table <- data.frame(x = 1:15, y = 1:15, z = 1:15)

	RUnit::checkException(simpleValueFill(original_table, c("x", "x", "DoesNotExist")))
}

testUseOnlySomeColumns <- function() {
    original_table <- data.frame(x = 1:15, y = rep(NA, 15), z = seq(2, 30, 2))
    only_use_xy <- c("x", "y")
    target_table <- data.frame(x = 1:15, y = 1:15, z = seq(2, 30, 2))

    filled_table <- simpleValueFill(original_table, only_use_xy)
    RUnit::checkEquals(target_table, filled_table)
}

testErrorForDuplicatedColumnNames <- function() {
    original_table <- data.frame(x = 1:15, y = 1:15)
    names(original_table) <- c("x", "x")
	RUnit::checkException(simpleValueFill(original_table, c("x", "x")))
}

testFillWithNonStandardColumnNames <- function() {
    original_table <- data.frame(x = 1:15, y = 1:15)
    names(original_table) <- c("001", "002")

    filled_table <- simpleValueFill(original_table, value_cols = c("001", "002"))

    RUnit::checkEquals(target = c("001", "002"), names(filled_table))
}

testAllValuesAreEmpty <- function() {
    original_table <- data.frame(x = as.numeric(rep(NA, 10)), y = as.numeric(rep(NA, 10)))

    filled_table <- simpleValueFill(original_table, c("x", "y"))

    RUnit::checkEquals(original_table, filled_table)
}

testOnMoreThanTwoColumnsWithOnlyNaNoNanIsReturned <- function() {
    value_table <- data.frame(x = c(1:5, NA, 7:10), y =  c(1:5, NA, 7:10), z = c(1:5, NA, 7:10))

    filled_table <- simpleValueFill(value_table, value_cols = c("x", "y", "z"))

    # check first if values are NA or NaN (is.na) and afterwards that non are NaN
    # (NA or NaN) and not NaN = NA and not NaN
    RUnit::checkTrue(TRUE %in% is.na(unlist(filled_table[6,])))
    RUnit::checkTrue(!(TRUE %in% is.nan(unlist(filled_table[6,]))))
}

testErrorOnOnlyOneColumn <- function() {
    value_table <- data.frame(x = 1:10)
    RUnit::checkException(simpleValueFill(value_table, value_cols = c("x")))
}

testErrorOnProvidedColsNotExisting <- function() {
    value_table <- data.frame(x = c(1:5, NA, 7:10), y =  c(1:5, NA, 7:10), z = c(1:5, NA, 7:10))
    RUnit::checkException(simpleValueFill(value_table, value_cols = c("x", "not_there")))
}
