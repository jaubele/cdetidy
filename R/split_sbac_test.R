#' Split SBAC data into ELA and mathematics
#'
#' Validates the SBAC test identifier and separates a combined SBAC dataset
#' into English language arts and mathematics datasets.
#'
#' @param data A data frame containing a `test` column, where `1` identifies
#'   English language arts and `2` identifies mathematics.
#'
#' @return A named list containing `ela` and `math` data frames.
#'
#' @export
split_sbac_test <- function(data) {
  
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE)
  }
  
  if (!"test" %in% names(data)) {
    stop(
      "SBAC data must include a `test` column.",
      call. = FALSE)
  }
  
  if (anyNA(data$test)) {
    stop(
      "SBAC `test` contains missing values.",
      call. = FALSE)
  }
  
  observed_test_values <- sort(
    unique(data$test))
  
  unexpected_test_values <- setdiff(
    observed_test_values,
    c(1L, 2L))
  
  if (length(unexpected_test_values) > 0L) {
    stop(
      "Unexpected SBAC test value(s): ",
      paste(unexpected_test_values, collapse = ", "),
      ". Expected only 1 for ELA and 2 for mathematics.",
      call. = FALSE)
  }
  
  if (!1L %in% observed_test_values) {
    stop(
      "No ELA records were found. Expected `test == 1`.",
      call. = FALSE)
  }
  
  if (!2L %in% observed_test_values) {
    stop(
      "No Math records were found. Expected `test == 2`.",
      call. = FALSE)
  }
  
  df_ela <- dplyr::filter(
    data,
    .data$test == 1L)
  
  df_math <- dplyr::filter(
    data,
    .data$test == 2L)
  
  if (nrow(df_ela) + nrow(df_math) != nrow(data)) {
    stop(
      "SBAC subject split did not preserve every input row.",
      call. = FALSE)
  }
  
  message(
    "SBAC split complete: ",
    nrow(df_ela),
    " ELA rows and ",
    nrow(df_math),
    " mathematics rows.")
  
  list(
    ela = df_ela,
    math = df_math)
}
