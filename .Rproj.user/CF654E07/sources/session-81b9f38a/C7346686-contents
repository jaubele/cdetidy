#' Verify that CDS and component columns are correctly padded
#'
#' Checks whether CDS-related columns (`county_code`, `district_code`, `school_code`, and `cds`)
#' are character type and have the expected number of digits (2, 5, 7, and 14, respectively).
#' Optionally drops the component columns after successful validation.
#'
#' @param df A data frame containing the CDS and its component columns.
#' @param drop_parts Logical. If `TRUE`, removes `county_code`, `district_code`, and `school_code`
#' after successful validation. Default is `TRUE`.
#'
#' @return Returns the input data frame (possibly with columns dropped).
#' Returns `FALSE` (invisibly) if validation fails. Messages are printed using the `cli` package.
#'
#' @details
#' - Each column must be present, of type character, and match its expected width.
#' - The expected lengths are:
#'   - `county_code`: 2 digits
#'   - `district_code`: 5 digits
#'   - `school_code`: 7 digits
#'   - `cds`: 14 digits
#'
#' @examples
#' df <- data.frame(
#'   county_code = "01",
#'   district_code = "12345",
#'   school_code = "6789012",
#'   cds = "01123456789012",
#'   stringsAsFactors = FALSE
#' )
#' verify_padded_cds(df)
#'
#' @export

verify_padded_cds <- function(df, drop_parts = TRUE) {
  expected_lengths <- c(
    county_code = 2,
    district_code = 5,
    school_code = 7,
    cds = 14
  )

  issues <- purrr::imap(expected_lengths, function(width, col) {
    if (!col %in% names(df)) {
      return(glue::glue("❌ Column '{col}' is missing."))
    }

    values <- df[[col]]
    if (!is.character(values)) {
      return(glue::glue("❌ Column '{col}' is not character (is {typeof(values)})."))
    }

    bad_n <- sum(nchar(values) != width, na.rm = TRUE)
    if (bad_n > 0) {
      return(glue::glue("⚠️ Column '{col}' has {bad_n} rows with incorrect length (expected {width})."))
    }

    NULL
  }) %>% purrr::compact()

  if (length(issues) > 0) {
    cli::cli_alert_danger("CDS post-padding validation failed:")
    cli::cli_bullets(issues)
    return(FALSE)
  }

  cli::cli_alert_success("✅ All CDS-related columns are correctly padded and validated.")

  # Drop component columns if validation succeeded and user requested cleanup
  if (drop_parts) {
    df <- df %>% select(-county_code, -district_code, -school_code)
    cli::cli_alert_info("📦 Dropped component columns: county_code, district_code, school_code")
  }

  return(df)
}
