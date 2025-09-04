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
#' @param show_examples Integer. If duplicates are found, print up to this many example offending key combinations (after de-duplicating). Set to 0 to suppress examples. Default is 10.
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

validate_primary_key <- function(data, key_cols, sample_n = 10000, full_run = FALSE, seed = 1234, show_examples = 10) {

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

    idx  <- sample.int(total_rows, sample_n)
    data <- data[idx, , drop = FALSE]

    cat("📊 Sampled ", sample_n, " of ", total_rows, " rows for primary key validation.\n")
    cat("🔍 To check the entire dataset, use: full_run = TRUE\n")
  } else {

    cat("📊 Checking all ", total_rows, " rows for primary key validation...\n")
  }

  # 3) NA check in key columns
  na_counts <- vapply(key_cols, function(k) sum(is.na(data[[k]])), integer(1))
  if (any(na_counts > 0)) {
    message("⚠️ NAs in key columns:")
    print(stats::setNames(as.integer(na_counts), key_cols))
  }

  # 4) Duplicate check
  dup_flag <- duplicated(data[, key_cols, drop = FALSE])
  dup_n    <- sum(dup_flag)

  if (dup_n == 0 && all(na_counts == 0)) {
    message("\033[32m✅ These columns [", paste(key_cols, collapse = ", "), "] comprise the primary key",
            if (!full_run && total_rows > sample_n) " (within this sample)", ".", "\033[0m")
    return(invisible(list(pass = TRUE, duplicates = 0, na = na_counts)))
  }

  # 5) Report duplicates (with examples)
  if (dup_n > 0) {
    message("\033[31m❌ ", dup_n, " duplicate row(s) by [", paste(key_cols, collapse = ", "), "].\033[0m")
    dups <- data[dup_flag, key_cols, drop = FALSE]
    if (nrow(dups) > show_examples) dups <- head(dups, show_examples)
    message("🔎 Example offending keys:")
    print(unique(dups))
  }

  invisible(list(pass = FALSE, duplicates = dup_n, na = na_counts))
}
