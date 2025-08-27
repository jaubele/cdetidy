#' Safely quote SQL Server identifiers
#'
#' Wraps a character string in square brackets for use as a SQL Server identifier.
#' Escapes any closing brackets within the string to avoid syntax errors.
#' Useful for safely referencing table or column names in dynamic SQL generation.
#'
#' @param x A character vector of table or column names.
#'
#' @return A character vector with each element wrapped in square brackets and escaped properly.
#'
#' @examples
#' sql_ident("table")           # "[table]"
#' sql_ident("column name")     # "[column name]"
#' sql_ident("some]name")       # "[some]]name]"
#'
#' @export

sql_ident <- function(x) paste0("[", gsub("]", "]]", x, fixed = TRUE), "]")
