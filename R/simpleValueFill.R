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
    original_names <- names(value_table)
    new_names <- make.names(original_names)
    out_table <- value_table %>%
        dplyr::mutate_at(dplyr::vars(value_cols), as.numeric) %>%
        data.table::as.data.table() %>%
        data.table::setnames(original_names, new_names)

    new_value_cols <- make.names(value_cols)
    var_pairs <- rje::powerSet(unique(new_value_cols)) %>%
        purrr::map(., function(x) if (length(x) == 2) return(x)) %>%
        Filter(f = Negate(is.null), x = .)

    correlation_table <- data.table::data.table(
        matrix(unlist(var_pairs),
               ncol = 2,
               byrow = TRUE))
    data.table::setnames(correlation_table, c("variable_1", "variable_2"))
    for (pair in var_pairs) {
        calculated_correlation <- out_table[, cor(get(pair[1]), get(pair[2]), use = "na.or.complete")]
        correlation_table[variable_1 == pair[1] & variable_2 == pair[2], correlation := calculated_correlation]
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
            pair_formula <- as.formula(paste(value.col, replace.column, sep = "~"))
            model <- out_table[, lm(formula = pair_formula, data = .SD)]
            out_table[is.na(get(value.col)) & !is.na(get(replace.column)), (value.col) := predict(model, .SD)]
        }
        out_table[is.na(get(value.col)), (value.col) := rowMeans(.SD, na.rm = TRUE), by = unselected_columns]
        out_table[is.nan(get(value.col)), (value.col) := NA]
    }
    out_table <- data.frame(out_table)
    data.table::setnames(out_table, original_names)
    return(out_table)
}
