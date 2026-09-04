#' Validate a recoded column
#'
#' Checks that each non-missing source value maps to exactly one non-missing
#' target value. Multiple source values may map to the same target value.
#' Optionally prints frequency and cross-tabulation tables for review.
#'
#' @param data A data frame containing the source and target columns.
#' @param source_col Character string naming the original source column.
#' @param target_col Character string naming the recoded target column.
#' @param show_table Logical. If `TRUE`, print source frequencies, a
#'   source-by-target cross-tabulation, and the target column structure.
#' @param fail_on_invalid Logical. If `TRUE`, stop when invalid mappings are
#'   found. If `FALSE`, issue a warning and return the validation results.
#'
#' @return Invisibly returns a list containing:
#' \describe{
#'   \item{pass}{Whether the recode is valid.}
#'   \item{source_col}{The source column name.}
#'   \item{target_col}{The target column name.}
#'   \item{problems}{A summary of source values with invalid mappings.}
#'   \item{problem_data}{Rows containing invalid source values.}
#' }
#'
#' @examples
#' example_data <- data.frame(
#'   source = c("Yes", "No", "All"),
#'   target = c(1L, 0L, 2L)
#' )
#'
#' validate_recode(
#'   example_data,
#'   source_col = "source",
#'   target_col = "target"
#' )
#'
#' @export
validate_recode <- function(data,
                            source_col,
                            target_col,
                            show_table = FALSE,
                            fail_on_invalid = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(source_col) || length(source_col) != 1L ||
      is.na(source_col)) {
    stop("`source_col` must be one non-missing character string.",
         call. = FALSE)
  }

  if (!is.character(target_col) || length(target_col) != 1L ||
      is.na(target_col)) {
    stop("`target_col` must be one non-missing character string.",
         call. = FALSE)
  }

  missing_cols <- setdiff(c(source_col, target_col), names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing recode column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  source <- data[[source_col]]
  target <- data[[target_col]]
  source_present <- !is.na(source)

  mapping <- tibble::tibble(
    source_value = source[source_present],
    target_value = target[source_present]
  ) |>
    dplyr::group_by(.data$source_value) |>
    dplyr::summarise(
      n_targets = dplyr::n_distinct(.data$target_value, na.rm = TRUE),
      has_missing_target = any(is.na(.data$target_value)),
      .groups = "drop"
    )

  problems <- mapping |>
    dplyr::filter(
      .data$n_targets != 1L | .data$has_missing_target
    )

  problem_values <- problems$source_value

  problem_data <- data[
    source_present & source %in% problem_values,
    ,
    drop = FALSE
  ]

  valid <- nrow(problems) == 0L

  if (isTRUE(show_table)) {
    cat("\nSource frequencies:", source_col, "\n")
    print(table(source, useNA = "ifany"))

    cat("\nRecode cross-tabulation:", source_col, "->", target_col, "\n")
    print(table(source, target, useNA = "ifany"))

    cat("\nTarget structure:", target_col, "\n")
    utils::str(target)
  }

  result <- list(
    pass = valid,
    source_col = source_col,
    target_col = target_col,
    problems = problems,
    problem_data = problem_data
  )

  if (valid) {
    message(
      "Valid recode: ",
      source_col,
      " maps consistently to ",
      target_col,
      "."
    )
  } else {
    invalid_values <- paste(problem_values, collapse = ", ")
    msg <- paste0(
      "Invalid recode from ",
      source_col,
      " to ",
      target_col,
      ". Problem source value(s): ",
      invalid_values
    )

    if (isTRUE(fail_on_invalid)) {
      stop(msg, call. = FALSE)
    }

    warning(msg, call. = FALSE)
  }

  invisible(result)
}
