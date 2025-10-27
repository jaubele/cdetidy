#' Compare suppression-affected columns across two datasets
#'
#' Uses the `star_scan()` utility to detect which columns contain suppressed values
#' (typically marked by `"*"`), and compares two data frames to identify changes in
#' suppression status across columns.
#'
#' @param df_prev A data frame representing the earlier or baseline dataset.
#' @param df_curr A data frame representing the newer or updated dataset.
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{new_suppressed_cols}{A list of column names that are newly suppressed in `df_curr` but not in `df_prev`}
#'   \item{stopped_being_suppressed}{Columns that were suppressed in `df_prev` but are no longer suppressed in `df_curr`}
#'   \item{unchanged_suppressed_cols}{Columns that are suppressed in both datasets}
#' }
#'
#' @details
#' This function relies on the `star_scan()` function being available in the environment.
#' By default, it looks for `"*"` values as indicators of suppression. This is useful for
#' QA comparisons of public CDE files across time or versions.
#'
#' @seealso [star_scan()]
#'
#' @examples
#' df1 <- data.frame(a = c(1, 2), b = c("*", 4), c = c("*", "*"))
#' df2 <- data.frame(a = c("*", "*"), b = c("3", "4"), c = c("*", "*"))
#' compare_suppression_columns(df1, df2)
#'
#' @export

# Requires your star_scan() util available in the session
compare_suppression_columns <- function(df_prev, df_curr) {
  collapse_vec <- function(x) if (length(x)) paste(sort(x), collapse = ", ") else "(none)"
  
  prev_cols <- star_scan(df_prev)$columns
  curr_cols <- star_scan(df_curr)$columns
  
  tibble::tibble(
    new_suppressed_cols       = collapse_vec(setdiff(curr_cols, prev_cols)),
    stopped_being_suppressed  = collapse_vec(setdiff(prev_cols, curr_cols)),
    unchanged_suppressed_cols = collapse_vec(intersect(prev_cols, curr_cols))
  )
}