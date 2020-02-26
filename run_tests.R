########################################################################################################################
devtools::load_all()

########################################################################################################################
full_test_suite <- RUnit::defineTestSuite("Full test suite", dirs = "RUnitTests")
RUnit::isValidTestSuite(full_test_suite)
runit <- RUnit::runTestSuite(full_test_suite)
RUnit::printTextProtocol(runit)


