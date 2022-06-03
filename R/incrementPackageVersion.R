#' Increment version (Major, Minor, Patch, Development) of a package DESCRIPTION
#' file
#'
#' @param source_path path of the package whose DESCRIPTION is to be incremented
#' @param incrementing_level Which level (Major, Minor, Patch, Development) of
#' the version number to increment
#' @export
#'
# TODO: Possible values as package constant
incrementPackageVersion <- function(source_path, incrementing_level) {
  possible_levels <- c("Major", "Minor", "Patch", "Development")
  if (!(incrementing_level %in% possible_levels)) {
    stop(
      "incrementing_level must be one of the following: \n",
      paste(possible_levels, collapse = ", ")
    )
  }

  # load description
  description_file <- dir(
    source_path,
    pattern = "^DESCRIPTION$",
    full.names = TRUE, recursive = TRUE
  )
  description_lines <- readLines(description_file)
  version_row_number <- which(stringr::str_detect(
    string = description_lines,
    pattern = "^Version:"
  ))
  version_line <- description_lines[version_row_number]

  # deconstruct information
  current_version <- stringr::str_match(
    version_line,
    pattern = "(?<=^Version: )[0-9]+\\.[0-9]+\\.[0-9]+(?:\\.[0-9]+)?$"
  )
  split_version_vector <- current_version %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist() %>%
    as.numeric()
  names(split_version_vector) <- possible_levels[
    1:length(split_version_vector)
  ]

  # increment and reconstruct information
  incrementing_function <- get(paste0(
    ".increment", incrementing_level,
    "Version"
  ))
  updated_version_vector <- incrementing_function(split_version_vector)


  incremented_version <- paste(updated_version_vector, collapse = ".")
  new_line <- paste0("Version: ", incremented_version)
  description_lines[version_row_number] <- new_line

  # write description
  writeLines(description_lines, description_file)
  cat(
    "Changed Version number of Package:\n",
    source_path,
    "\n from:\t", current_version,
    "\n to:\t", incremented_version,
    "\n"
  )
}

.incrementDevelopmentVersion <- function(split_version_vector) {
  is_out_of_development <- TRUE %in% (unlist(split_version_vector)[1:3] > 0)
  if (is_out_of_development) {
    stop(
      "Should not increment 'Development' version of package which already ",
      "left development and ",
      "has a Main/Minor/Patch version"
    )
  }
  split_version_vector["Development"] <- split_version_vector["Development"] + 1
  return(split_version_vector)
}

.incrementPatchVersion <- function(split_version_vector) {
  released_version_vector <- .dropDevelopmentVersion(split_version_vector)
  released_version_vector["Patch"] <- released_version_vector["Patch"] + 1
  return(released_version_vector)
}

.incrementMinorVersion <- function(split_version_vector) {
  released_version_vector <- .dropDevelopmentVersion(split_version_vector)
  released_version_vector["Patch"] <- 0
  released_version_vector["Minor"] <- released_version_vector["Minor"] + 1
  return(released_version_vector)
}

.incrementMajorVersion <- function(split_version_vector) {
  released_version_vector <- .dropDevelopmentVersion(split_version_vector)
  released_version_vector["Patch"] <- 0
  released_version_vector["Minor"] <- 0
  released_version_vector["Major"] <- released_version_vector["Major"] + 1
  return(released_version_vector)
}

.dropDevelopmentVersion <- function(split_version_vector) {
  without_development <- split_version_vector[names(split_version_vector) !=
                                                "Development"]
  return(without_development)
}
