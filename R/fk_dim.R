#' Generate foreign key name for a dimension table
#'
#' Appends the suffix `"_dim"` to a given base name, typically to represent a foreign key column
#' that links to a dimension table.
#'
#' @param name A character string representing the base name of the dimension (e.g., `"student"`, `"school"`).
#'
#' @return A character string with `"_dim"` appended (e.g., `"student_dim"`).
#'
#' @export


fk_dim <- function(name) paste0(name, "_dim")
