########################################################################################################################
codetools::checkUsagePackage("MyUtilities", all = TRUE)
lintr::lint_package("../MyUtilities")
cyclocomp::cyclocomp_package("MyUtilities")
cleanr::check_package("../MyUtilities")

#styler::style_file()


