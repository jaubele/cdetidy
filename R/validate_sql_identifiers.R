#' Validate SQL-safe table or column names
#'
#' Checks that each name in a character vector follows SQL identifier rules:
#' - Starts with a letter or underscore
#' - Contains only letters, numbers, or underscores
#' - Does not exceed a specified maximum length
#'
#' @param name_vec A character vector of names to validate.
#' @param type A string indicating what type of identifier is being checked. Must be `"column"` or `"table"`. Default is `"column"`.
#' @param max_length Maximum number of characters allowed in a name. Default is 63.
#'
#' @return Invisibly returns `NULL`. Raises an error if any invalid names are found.
#'
#' @details
#' - This function enforces safe naming for SQL Server and PostgreSQL-style identifiers.
#' - Colored error messages are printed for easier debugging in interactive sessions.
#'
#' @examples
#' validate_sql_identifiers(c("student_id", "_school_code"))  # ✅ valid
#' validate_sql_identifiers(c("123bad", "with space"), type = "column")  # ❌ will error
#'
#' @export

validate_sql_identifiers <- function(name_vec, type = c("column", "table"), max_length = 63) {
  type <- match.arg(type)

  # Pattern: starts with letter or underscore, and contains only letters, numbers, or underscores
  invalid_pattern <- !grepl("^[A-Za-z_][A-Za-z0-9_]*$", name_vec)
  too_long <- nchar(name_vec) > max_length

  if (any(invalid_pattern)) {
    stop("\033[31m❌ Invalid SQL ", type, " name(s): ",
         paste(name_vec[invalid_pattern], collapse = ", "),
         "\n⚠️ Must start with a letter or underscore, and contain only letters, numbers, or underscores.\033[0m")
  }

  if (any(too_long)) {
    stop("\033[31m❌ ", type, " name(s) exceed ", max_length, " characters: ",
         paste(name_vec[too_long], collapse = ", "),
         "\n⚠️ Consider shortening these names for SQL compatibility.\033[0m")
  }
}
