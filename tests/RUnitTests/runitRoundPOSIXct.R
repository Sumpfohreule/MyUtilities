testThatMinuteValuesAreRounded <- function() {
    test_values <- as.POSIXct(
        c("2000-01-01 12:05:00",
          "2000-01-01 12:07:29",
          "2000-01-01 12:07:30"),
        tz = "UTC")

    rounded <- roundPOSIXct(test_values,
                            60 * 5)

    RUnit::checkEquals(c("2000-01-01 12:05:00", "2000-01-01 12:05:00", "2000-01-01 12:10:00"),
                       as.character(rounded))
}