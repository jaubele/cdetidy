#' Compare column names between two data frames
#'
#' Compares the column names of two data frames and prints a summary of whether they are identical,
#' along with the specific variables that are present in one but not the other. This function is useful
#' for checking schema consistency across datasets, especially in ETL or QA workflows.
#'
#' @param df1 The first data frame to compare.
#' @param df2 The second data frame to compare.
#'
#' @return A tibble summarizing the comparison (indicating whether the columns are identical),
#' and printed messages that describe which columns are unique to each data frame. No object is returned explicitly.
#'
#' @examples
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 1, c = 3)
#' compare_variable_names(df1, df2)
#'
#' @export

compare_variable_names <- function(df1, df2) {
  name1 <- deparse(substitute(df1))
  name2 <- deparse(substitute(df2))

  cols1 <- names(df1)
  cols2 <- names(df2)

  identical_cols <- identical(cols1, cols2)
  diff_1_to_2 <- setdiff(cols1, cols2)
  diff_2_to_1 <- setdiff(cols2, cols1)

  # Create and print the summary tibble
  summary_df <- tibble(
    Comparison = paste(name1, "vs", name2),
    Identical = identical_cols
  )

  # Print differences as separate messages
  if (length(diff_1_to_2) > 0) {
    cat("\nIn", name1, "but not in", name2, ":\n", paste(diff_1_to_2, collapse = ", "), "\n")
  } else {
    cat("\nNo variables found in", name1, "that are missing from", name2, "\n")
  }

  if (length(diff_2_to_1) > 0) {
    cat("\nIn", name2, "but not in", name1, ":\n", paste(diff_2_to_1, collapse = ", "), "\n")
  } else {
    cat("\nNo variables found in", name2, "that are missing from", name1, "\n")
  }
}
