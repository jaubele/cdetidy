#' Validate that an old-to-new group mapping is one-to-one
#'
#' Checks whether each unique value in the `old_col` maps to exactly one unique value in the `new_col`,
#' and vice versa. Useful for validating recode tables, lookup joins, or classification maps before applying them.
#' Uses `janitor::tabyl()` to cross-tabulate values and ensures each row and column contains only one match.
#'
#' @param data A data frame containing the columns to be compared.
#' @param old_col The unquoted name of the column representing the original values (e.g., codes or old labels).
#' @param new_col The unquoted name of the column representing the new mapped values.
#'
#' @return Invisibly returns `TRUE` if the mapping is one-to-one; otherwise `FALSE`.
#' Prints a success message if valid, or detailed messages if violations are found.
#'
#' @examples
#' \dontrun{
#' validate_group_mapping_tabyl(data, old_code, new_label)
#' }
#'
#' @export

validate_group_mapping_tabyl <- function(data, old_col, new_col) {
  require(janitor)
  require(dplyr)

  cross_tab <- data %>%
    tabyl({{ old_col }}, {{ new_col }})

  tab_matrix <- as.matrix(cross_tab[, -1])  # Remove the old_col label column

  # Check how many values > 0 in each row and each column
  row_violations <- which(rowSums(tab_matrix > 0) != 1)
  col_violations <- which(colSums(tab_matrix > 0) != 1)

  if (length(row_violations) == 0 && length(col_violations) == 0) {
    message("\033[32m✅ Each original value maps to exactly one new value.\033[0m")
    return(invisible(TRUE))
  } else {
    message("\033[31m❌ Detected unexpected mappings:\033[0m")
    if (length(row_violations) > 0) {
      message("→ ", length(row_violations), " original value(s) map to multiple new values.")
    }
    if (length(col_violations) > 0) {
      message("→ ", length(col_violations), " new value(s) receive multiple original values.")
    }
    return(invisible(FALSE))
  }
}
