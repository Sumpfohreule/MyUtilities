#' Converts function names to snake_case
#'
#' Given a file all function definitions are converted to snake_case including
#' the file name. Underscores are prepended if it would otherwise overwrite a
#' function.
#' The original file and function will be kept, however with deprecation
#' information
#' Only works function definition is in one line
#'
#' @param function_file Path to the file containing the function definition
#' @export
make_snake_case_function <- function(function_file) {
    FUNCTION_REGEX <- " *(?:<-|=) *function *\\("
    NAME_REGEX <- "[[:alpha:]_.][[:alnum:]_.]*"
    DETECT_LINE_REGEX <- paste0("^ *[^#] *", NAME_REGEX, FUNCTION_REGEX)
    MATCH_NAME_REGEX <- paste0(NAME_REGEX, "(?=", FUNCTION_REGEX, ")")

    # Read file
    original_contents <- readLines(function_file)

    # Detect function definitions
    line_has_definition <- original_contents %>%
        stringr::str_detect(DETECT_LINE_REGEX)

    # Stop if none found
    if (all(isFALSE(line_has_definition))) {
        stop("No function definitions were found in file: ", function_file)
    }

    # Rename found definitions
    new_contents <- original_contents %>%
        purrr::map_if(
            .p = line_has_definition,
            .f = ~{
                new_function_name <- stringr::str_match(
                    .x,
                    MATCH_NAME_REGEX) %>%
                    as.character() %>%
                    make_snake_case()
                mapped_value <- stringr::str_replace(
                    .x,
                    pattern = MATCH_NAME_REGEX,
                    replacement = new_function_name)
                return(mapped_value)
            }) %>%
        unlist()

    # Create renamed file with new contents
    new_file <- function_file %>%
        basename() %>%
        stringr::str_match(NAME_REGEX) %>%
        make_snake_case() %>%
        sprintf("%s/%s.R",
                dirname(function_file),
                .)

    while(file.exists(new_file)) {
        # Prepend _ if file exists to make sure nothing is overwriten here
        new_file <- paste0("_", new_file)
    }
    writeLines(new_contents, new_file)

    # Comment out old function and add deprecated information with call to new
    # function
    original_commented_out <- original_contents %>%
        purrr::map_chr(~ paste0("#", .x))

    deprecated_functions <- original_contents[line_has_definition] %>%
        purrr::map(~ {
            new_function_name <- .x %>%
                stringr::str_match(MATCH_NAME_REGEX) %>%
                make_snake_case()
            passthrough_parameters <- .x %>%
                stringr::str_replace(DETECT_LINE_REGEX, "") %>%
                stringr::str_replace(" *\\) *\\{ *", "") %>%
                stringr::str_split(",") %>%
                unlist() %>%
                purrr::map_chr(trimws) %>%
                purrr::map_chr(stringr::str_match,
                               pattern = paste0("(?<=^|,)", NAME_REGEX)) %>%
                purrr::map_chr(~ paste0(.x, " = ", .x)) %>%
                paste0(collapse = ", ")
            call_of_new_function <- paste0(
                new_function_name, " <- function(", passthrough_parameters, ")"
            )

            c(.x,
              paste0("\t.Deprecated(\"", new_function_name, "\")"),
              paste0("\t", call_of_new_function),
              "}")
        }) %>% unlist()
    writeLines(
        text = c(original_commented_out, deprecated_functions),
        con = function_file)
}

#' Converts a string from camelCase to snake_case
#'
#' @param string A string or vector which should be converted to camel_case
#' @return The string converted to snake_case
make_snake_case <- function(camel_case_string) {
    # Regex allows names starting with _ or a letter followed by
    # alphanumeric values or an _
    FUNCTION_NAME_REGEX <- "^[[:alpha:]_.][[:alnum:]_.]*$"
    camel_case_string %>%
        stringr::str_detect(FUNCTION_NAME_REGEX) %>%
        all() %>%
        assertthat::assert_that(
            msg = "At least one string is not a correct function string")

    camel_case_string %>%
        # Replace all capital letters with an underscore and its lower case
        gsub(pattern = "([A-Z0-9]+)",
             replacement = "_\\L\\1",
             x = .,
             perl = TRUE) %>%
        # Remove potential underscore from the beginning
        sub(pattern = "^_",
            replacement = "",
            x = ., perl = TRUE)
}


