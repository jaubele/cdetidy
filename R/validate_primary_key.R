#' Validate primary key uniqueness in a dataset
#'
#' Checks whether a set of columns uniquely identify rows in a data frame.
#' Can be run on a full dataset or a sampled subset to improve performance on large data.
#'
#' @param data A data frame to check.
#' @param key_cols A character vector of column names that should act as the primary key.
#' @param sample_n Integer. Number of rows to sample for validation (if `full_run = FALSE`). Default is 10,000.
#' @param full_run Logical. If `TRUE`, checks the entire dataset. Default is `FALSE`.
#' @param seed An integer used to seed the random sample (only applies if `full_run = FALSE`). Default is 1234.
#'
#' @return Invisibly returns `NULL`. Prints a success or failure message indicating whether the key uniquely identifies rows.
#'
#' @details
#' - Columns listed in `key_cols` must all exist in `data`.
#' - If `full_run = FALSE` and the dataset has more than `sample_n` rows, a sample is used.
#' - Uses `anyDuplicated()` for fast duplicate detection.
#' - Messages use colored console output (green = valid, red = invalid).
#'
#' @examples
#' validate_primary_key(mydata, key_cols = c("student_id", "year"))
#'
#' @export

validate_primary_key <- function(data, key_cols, sample_n = 10000, full_run = FALSE, seed = 1234) {

  data <- as.data.frame(data)  # ensure base R compatibility

  # Column check
  if (!all(key_cols %in% names(data))) {
    stop("❌ One or more key columns are not present in the dataset: ",
         paste(setdiff(key_cols, names(data)), collapse = ", "))
  }

  total_rows <- nrow(data)

  # Sampling logic
  if (!full_run && total_rows > sample_n) {
    set.seed(seed)
    data <- data[sample(total_rows, sample_n), , drop = FALSE]
    cat("📊 Sampled ", sample_n, " of ", total_rows, " rows for primary key validation.\n")
    cat("🔍 To check the entire dataset, use: full_run = TRUE\n")
  } else if (full_run) {
    cat("📊 Checking all ", total_rows, " rows for primary key validation...\n")
  } else {
    cat("📊 Dataset has only ", total_rows, " rows — using full dataset for check.\n")
  }

  # Check for duplicates
  duplicated_exists <- anyDuplicated(data[, key_cols, drop = FALSE]) > 0

  if (duplicated_exists) {
    message("\033[31m❌ The combination of columns [", paste(key_cols, collapse = ", "), "] does NOT uniquely identify rows.\033[0m")
    message("→ Try adding more columns to define a composite primary key.")
  } else {
    message("\033[32m✅ These columns [", paste(key_cols, collapse = ", "), "] comprise the primary key",
            if (!full_run && total_rows > sample_n) " (within this sample)", ".", "\033[0m")
  }

  invisible(NULL)
}
