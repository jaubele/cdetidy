#' Flag schema changes by comparing to a prior-year archive
#'
#' Compares the current schema definition against an archived schema log from a specified year,
#' and flags tables for rebuild if any columns have changed in SQL type, nullability, or primary key status.
#' Adds a `rebuild_flag` column to the original schema data frame.
#'
#' @param schema_df A data frame representing the current schema, typically generated from a table structure.
#' @param year A 4-digit year (as numeric or character) used to locate the archived schema log.
#'
#' @return A copy of `schema_df` with an added `rebuild_flag` column:
#' - `1` if the table requires rebuild due to schema changes
#' - `0` otherwise
#'
#' @details
#' - The archived schema must exist at a standardized location on the T drive.
#' - Column names compared include: `sql_type`, `null_flag`, and `is_primary_key`.
#' - Uses an internal normalization step (`normalize_for_compare()`) to align formats before comparison.
#'
#' @export

flag_schema_changes <- function(schema_df, year) {
  if (is.numeric(year)) year <- as.character(year)
  stopifnot(!is.null(year), nchar(year) == 4)

  archive_path <- file.path(
    "T:/Data Warehouse/Warehouse Ready Files/schema_files/Archive",
    paste0("primary_sql_schema_log_", year, ".csv")
  )
  if (!file.exists(archive_path)) {
    stop("❌ Archived schema log not found for year ", year, " at: ", archive_path)
  }

  # Remove timestamp note row if present
  schema_archived <- readr::read_csv(archive_path, show_col_types = FALSE) %>%
    dplyr::filter(!is.na(column_name))  # legacy safety

  # normalize BOTH sides the same way
  schema_curr_norm <- normalize_for_compare(schema_df)
  schema_old_norm  <- normalize_for_compare(schema_archived)

  # join + compare
  joined <- dplyr::left_join(
    schema_curr_norm, schema_old_norm,
    by = c("table_name","column_name"), suffix = c("_curr","_old")
  ) %>%
    dplyr::mutate(
      schema_changed = is.na(sql_type_old) |
        sql_type_curr       != sql_type_old |
        null_flag_curr      != null_flag_old |
        is_primary_key_curr != is_primary_key_old
    )

  tables_to_rebuild <- joined %>%
    dplyr::filter(schema_changed) %>%
    dplyr::pull(table_name) %>%
    unique()

  # add flag back to the ORIGINAL df (with all columns preserved)
  schema_flagged <- schema_df %>%
    dplyr::mutate(rebuild_flag = dplyr::if_else(table_name %in% tables_to_rebuild, 1, 0))

  cat("🔍 Compared current schema with archived ", year, " log.\n", sep = "")
  cat("🛠️  Flagged ", length(tables_to_rebuild), " table(s) for rebuild (type, nullability, or PK change).\n", sep = "")

  return(schema_flagged)

}
