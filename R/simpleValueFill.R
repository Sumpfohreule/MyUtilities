########################################################################################################################
#' Fills missing values starting with the best correlation
#'
#' Correlations between value columns are tested and missing values are replaced beginning with the best correlation.
#'
#' @param value_table A data.table which contains the values to be filled and for replacement
#' @param value_cols column names of the columns which are to be filled and used for replacement
#' @param min_correlation Minimum correlation factor between columns to use it for filling
#' @export
#'
simpleValueFill <- function(value_table, value_cols, min_correlation = 0.9) {
    if (length(value_cols) <= 1) {
        stop("To fill values, at least two value_cols need to be provided")
    }
    original_names <- names(value_table)
    if (FALSE %in% (value_cols %in% original_names)) {
        stop("At least one of the value_cols was not found in value_table")
    }
    new_names <- make.names(original_names)
    out_table <- value_table %>%
        dplyr::mutate_at(tidyselect::all_of(value_cols), as.numeric) %>%
        data.table::as.data.table() %>%
        data.table::setnames(original_names, new_names) %>%
        data.frame()

    new_value_cols <- make.names(value_cols)
    var_pairs <- rje::powerSet(unique(new_value_cols)) %>%
        purrr::map(., function(x) if (length(x) == 2) return(x)) %>%
        Filter(f = Negate(is.null), x = .)

    correlation_table <- data.table::data.table(
        matrix(unlist(var_pairs),
               ncol = 2,
               byrow = TRUE)) %>%
        data.table::setnames(c("variable_1", "variable_2")) %>%
        mutate(correlation = as.numeric(NA))
    for (pair in var_pairs) {
        calculated_correlation <- out_table %>%
            summarise(cor(
                x = !!as.symbol(pair[1]),
                y = !!as.symbol(pair[2]),
                use = "na.or.complete"))
        index <- correlation_table[, "variable_1"] == pair[1] & correlation_table[, "variable_2"] == pair[2]
        correlation_table[index, "correlation"] <- calculated_correlation
    }
    correlation_table <- correlation_table %>%
        dplyr::filter(correlation >= min_correlation) %>%
        data.table::data.table()

    unselected_columns <- setdiff(new_names, new_value_cols)
    for (value.col in new_value_cols) {
        reduced_correlation <- correlation_table %>%
            dplyr::filter(variable_1 == value.col | variable_2 == value.col) %>%
            dplyr::mutate(variable_1 = dplyr::if_else(
                variable_1 == value.col,
                true = variable_2,
                false = variable_1)) %>%
            dplyr::select(-variable_2) %>%
            dplyr::arrange(dplyr::desc(correlation))

        for (replace.column in reduced_correlation[, "variable_1"]) {
            replacement_rows <- out_table %>%
                mutate(temp_na_replace = is.na(!!as.symbol(value.col)) & !is.na(!!as.symbol(replace.column))) %>%
                pull(temp_na_replace)
            if (sum(replacement_rows) > 0) {
                pair_formula <- as.formula(paste(value.col, replace.column, sep = "~"))
                model <- out_table %>%
                    lm(formula = tidyselect::all_of(pair_formula), data = .)
                replacement_predictions <- model %>%
                    predict(out_table[replacement_rows, ])
                out_table[replacement_rows, value.col] <- replacement_predictions
            }
        }
    }
    row_means <- out_table %>%
        mutate(temp_row_means = rowMeans(select(., tidyselect::all_of(new_value_cols)), na.rm = TRUE)) %>%
        mutate(temp_row_means = if_else(
            condition = is.nan(temp_row_means),
            true = as.numeric(NA),
            false = temp_row_means
        )) %>%
        pull(temp_row_means)

    for (value_column in new_value_cols) {
        na_values <- out_table %>%
            pull(value_column) %>%
            is.na()
        out_table[na_values, value_column] <- row_means[na_values]
    }
    data.table::setnames(out_table, original_names)
    return(out_table)
}
