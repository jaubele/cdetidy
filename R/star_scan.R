#' Scan a data frame for pattern-matching values (e.g., "*")
#'
#' This function scans all columns in a data frame and identifies which columns
#' contain values matching a given pattern (e.g., `"*"` for suppression). It returns
#' a list with the names of the columns containing matches and a summary of how many
#' matches were found in each.
#'
#' @param df A data frame to scan.
#' @param pattern A character string containing the pattern to match. Default is `"*"`.
#' @param fixed Logical. If TRUE (default), `pattern` is treated as a fixed string. If FALSE, it's treated as a regular expression.
#' @param print Logical. If TRUE (default), prints a message showing which columns matched.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{columns}{A character vector of column names containing matches.}
#'   \item{summary}{A tibble summarizing how many matches were found per column.}
#' }
#'
#' @examples
#' chronic_example <- data.frame(
#'   enrollment = c("100", "75", "50"),
#'   chronic_count = c("10", "*", "5"),
#'   chronic_rate = c("10.0", "*", "10.0")
#' )
#'
#' scan_result <- star_scan(chronic_example)
#' scan_result$columns
#' scan_result$summary
#'
#' @export

star_scan <- function(df, pattern = "*", fixed = TRUE, print = TRUE) {
  # Internal helpers (scoped inside the wrapper)
  columns_with_star <- function(x) {
    has_star <- purrr::map_lgl(x, ~ any(grepl(pattern, as.character(.x), fixed = fixed, useBytes = TRUE), na.rm = TRUE))
    names(x)[has_star]
  }
  star_summary <- function(x) {
    tibble::tibble(
      column = names(x),
      n_star = purrr::map_int(x, ~ sum(grepl(pattern, as.character(.x), fixed = fixed, useBytes = TRUE), na.rm = TRUE))
    ) |>
      dplyr::filter(.data$n_star > 0) |>
      dplyr::arrange(dplyr::desc(.data$n_star))
  }
  
  cols <- columns_with_star(df)
  summ <- star_summary(df)
  
  if (print) {
    pat_disp <- if (fixed) pattern else paste0("/", pattern, "/")
    if (length(cols) == 0) {
      message("No matches found for ", pat_disp, ".")
    } else {
      message("Columns containing ", pat_disp, ": ", paste(cols, collapse = ", "))
    }
  }
  
  list(columns = cols, summary = summ)
}
