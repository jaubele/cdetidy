#' Split SBAC test data into ELA and Math datasets
#'
#' Splits a dataset with a `"test"` column (coded as 1 = ELA, 2 = Math) into two separate data frames.
#' Assigns the resulting datasets to the global environment using names based on a user-defined base name.
#'
#' @param data A data frame containing a `"test"` column with values `1` (ELA) and `2` (Math).
#' @param base_name A base string used to generate object names. Resulting objects will be named
#' `<base_name>_ela` and `<base_name>_math`.
#'
#' @return Invisibly returns a list with two components:
#' - `ela`: The subset of rows with `test == 1`
#' - `math`: The subset of rows with `test == 2`
#'
#' @details
#' - Rows with `test == 1` are assumed to be ELA records; `test == 2` are assumed to be Math.
#' - Warnings are printed if unexpected values are present in either subset.
#' - Resulting datasets are assigned to the global environment with descriptive names.
#' - Use `invisible()` to suppress printing if calling programmatically.
#'
#' @examples
#' \dontrun{
#' split_sbac_test(sbac_data, base_name = "sbac23")
#' # Creates sbac23_ela and sbac23_math in your environment
#' }
#'
#' @export

split_sbac_test <- function(data, base_name) {
  if (!"test" %in% names(data)) {
    stop("❌ The dataframe must include a 'test' column.")
  }

  # Split into ELA and Math
  df_ela <- dplyr::filter(data, test == 1)
  df_math <- dplyr::filter(data, test == 2)

  # Check that each split has only one test value
  test_vals_ela <- unique(df_ela$test)
  test_vals_math <- unique(df_math$test)

  if (length(test_vals_ela) != 1 || test_vals_ela != 1) {
    warning("⚠️ Unexpected test value(s) in ELA split: ", paste(test_vals_ela, collapse = ", "))
  }
  if (length(test_vals_math) != 1 || test_vals_math != 2) {
    warning("⚠️ Unexpected test value(s) in Math split: ", paste(test_vals_math, collapse = ", "))
  }

  # Create object names
  name_ela <- paste0(base_name, "_ela")
  name_math <- paste0(base_name, "_math")

  # Assign to global environment
  assign(name_ela, df_ela, envir = .GlobalEnv)
  assign(name_math, df_math, envir = .GlobalEnv)

  # Print confirmation
  cli::cli_h1(paste0("📤 Split Complete for ", base_name))
  cli::cli_alert_success(paste0("✅ Assigned ", name_ela, " — ", nrow(df_ela), " rows, ", ncol(df_ela), " cols."))
  cli::cli_alert_success(paste0("✅ Assigned ", name_math, " — ", nrow(df_math), " rows, ", ncol(df_math), " cols."))

  invisible(list(ela = df_ela, math = df_math))
}
