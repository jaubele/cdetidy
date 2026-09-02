#' Validate that each source group maps to one standardized group
#'
#' Checks a many-to-one mapping: each source value must map to exactly one
#' standardized value, but multiple synonymous source values may map to the
#' same standardized value. Missing standardized values are treated as invalid
#' when the source value is non-missing.
#'
#' @param data A data frame containing the columns to be compared.
#' @param old_col The unquoted name of the column representing the original values (e.g., codes or old labels).
#' @param new_col The unquoted name of the column representing the new mapped values.
#'
#' @return Invisibly returns `TRUE` if every source value has exactly one
#'   non-missing target; otherwise `FALSE`. Invalid source values are attached
#'   as the `problems` attribute.
#'
#' @examples
#' \dontrun{
#' validate_group_mapping_tabyl(data, old_code, new_label)
#' }
#'
#' @export

validate_group_mapping_tabyl <- function(data, old_col, new_col) {
  old_quo <- rlang::enquo(old_col)
  new_quo <- rlang::enquo(new_col)
  old_name <- rlang::as_name(old_quo)
  new_name <- rlang::as_name(new_quo)

  missing_cols <- setdiff(c(old_name, new_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing mapping column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  mapping <- data |>
    dplyr::filter(!is.na(.data[[old_name]])) |>
    dplyr::group_by(.data[[old_name]]) |>
    dplyr::summarise(
      n_targets = dplyr::n_distinct(.data[[new_name]], na.rm = TRUE),
      has_missing_target = any(is.na(.data[[new_name]])),
      .groups = "drop"
    )

  problems <- mapping |>
    dplyr::filter(n_targets != 1L | has_missing_target)
  valid <- nrow(problems) == 0L

  if (valid) {
    message("\033[32m✅ Each source value maps to exactly one standardized value.\033[0m")
  } else {
    message("\033[31m❌ Source values with missing or multiple mappings detected:\033[0m")
    print(problems)
  }

  invisible(structure(valid, problems = problems))
}
