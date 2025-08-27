#' Check suppression dependency across numeric columns
#'
#' Audits a data frame for suppression behavior (e.g., values represented by "*") across specified numeric columns.
#' Specifically, it checks whether suppression in a designated "trigger" column (e.g., cohort size) is consistently
#' applied when other numeric columns are suppressed — a common dependency rule in public education data releases.
#'
#' @param df A data frame to check for suppression patterns.
#' @param numeric_cols A character vector of column names expected to contain suppressible numeric values (e.g., counts or rates).
#' @param trigger_col The name of the column that should control or indicate suppression (default is `"cohort_students"`).
#' @param return_rows Logical. If `TRUE`, the function returns suppressed row subsets in addition to the summary. Default is `FALSE`.
#'
#' @return A tibble summarizing suppression dependencies, or a list containing the summary and detailed row subsets
#' if `return_rows = TRUE`. The summary includes:
#' - `all_trigger_suppressed_in_suppressed_rows`: Logical flag indicating if all suppressed rows also have suppression in the trigger column.
#' - `only_trigger_suppressed_rows_found`: Count of rows where only the trigger column is suppressed.
#' - `other_suppressed_when_trigger_not`: Count of rows where other columns are suppressed but the trigger column is not.
#'
#' @export

check_suppression_dependency <- function(df, numeric_cols, trigger_col = "cohort_students", return_rows = FALSE) {
  # 1. All suppressed rows: any "*" in numeric columns
  suppressed_rows <- df %>%
    filter(if_any(all_of(numeric_cols), ~ str_detect(.x, "\\*")))

  # 2. Check if ALL suppressed rows have "*" in trigger_col
  all_trigger_suppressed <- all(suppressed_rows[[trigger_col]] == "*")

  # 3. Only trigger_col is suppressed
  only_trigger_suppressed_rows <- df %>%
    filter(.data[[trigger_col]] == "*") %>%
    filter(if_all(setdiff(numeric_cols, trigger_col), ~ .x != "*"))

  only_trigger_suppressed <- nrow(only_trigger_suppressed_rows)

  # 4. Other suppression without trigger_col suppressed
  others_suppressed_but_not_trigger_rows <- df %>%
    filter(.data[[trigger_col]] != "*") %>%
    filter(if_any(setdiff(numeric_cols, trigger_col), ~ str_detect(.x, "\\*")))

  others_suppressed_but_not_trigger <- nrow(others_suppressed_but_not_trigger_rows)

  # Base summary
  summary_tbl <- tibble::tibble(
    all_trigger_suppressed_in_suppressed_rows = all_trigger_suppressed,
    only_trigger_suppressed_rows_found = only_trigger_suppressed,
    other_suppressed_when_trigger_not = others_suppressed_but_not_trigger
  )

  # Return with or without attached rows
  if (return_rows) {
    return(list(
      summary = summary_tbl,
      suppressed_rows = suppressed_rows,
      only_trigger_suppressed_rows = only_trigger_suppressed_rows,
      others_suppressed_but_not_trigger_rows = others_suppressed_but_not_trigger_rows
    ))
  } else {
    return(summary_tbl)
  }
}
