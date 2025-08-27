#' Generate SQL CREATE TABLE statements from schema metadata
#'
#' Converts a flagged schema log (e.g., from `sql_schema_log()` or `flag_schema_changes()`)
#' into SQL `CREATE TABLE` statements for each table in the dataset. Includes metadata headers,
#' optional drop-and-rebuild behavior, and primary key constraints.
#'
#' @param schema_flagged A data frame representing schema metadata. Must include the following columns:
#' - `table_name`, `column_name`, `sql_type`, `null_flag`
#' - `is_primary_key`, `is_foreign_key`, `foreign_key_ref`
#' - `user_note`, `user`, `timestamp`, `rebuild_flag`
#'
#' @return A character vector of SQL code blocks, one per table, formatted with metadata comments and valid T-SQL.
#'
#' @details
#' - Each block is prefixed with a detailed comment including who created the schema, when, and for what purpose.
#' - If any column in a table has `rebuild_flag == 1`, the block starts with `DROP TABLE IF EXISTS`.
#' - Otherwise, the block is wrapped in an `IF NOT EXISTS` check to avoid duplicate creation.
#' - Primary keys are included via `CONSTRAINT pk_<table_name> PRIMARY KEY (...)`.
#'
#' @examples
#' \dontrun{
#' schema_log <- readr::read_csv("primary_sql_schema_log.csv")
#' flagged <- flag_schema_changes(schema_log, year = 2023)
#' sql_blocks <- sql_table_coder(flagged)
#' cat(sql_blocks[1])
#' }
#'
#' @export

sql_table_coder <- function(schema_flagged) {
  # Defensive check
  required_cols <- c(
    "table_name","column_name","sql_type","null_flag",
    "is_primary_key","is_foreign_key","foreign_key_ref",
    "user_note","user","timestamp","rebuild_flag"
  )
  missing_cols <- setdiff(required_cols, names(schema_flagged))
  if (length(missing_cols) > 0) {
    stop("❌ Schema log is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Format timestamp as MM/DD/YYYY
  # Format timestamp as MM/DD/YYYY (robust to multiple input formats)
  schema_flagged <- schema_flagged %>%
    dplyr::mutate(
      ts_parsed = suppressWarnings(as.POSIXct(
        timestamp,
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
          "%Y-%m-%d",
          "%m/%d/%Y %H:%M:%OS", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
          "%m/%d/%Y"
        )
      )),
      timestamp_fmt = ifelse(
        is.na(ts_parsed),
        as.character(timestamp),            # fallback: leave as-is if unparseable
        format(ts_parsed, "%m/%d/%Y")
      ),
      rebuild_flag = dplyr::if_else(
        is.na(as.numeric(rebuild_flag)), 0, as.numeric(rebuild_flag)
      )
    ) %>%
    dplyr::select(-ts_parsed)

  # Group by table_name (deterministic order)
  table_names  <- sort(unique(schema_flagged$table_name))
  table_groups <- lapply(table_names, function(tnm) {
    schema_flagged %>% dplyr::filter(.data$table_name == tnm)
  })

  # Build SQL code block for each table
  sql_blocks <- purrr::map_chr(table_groups, function(tbl_df) {
    table_name   <- unique(tbl_df$table_name)
    column_count <- nrow(tbl_df)
    note         <- paste(unique(tbl_df$user_note), collapse = " | ")
    created_by   <- paste(unique(tbl_df$user), collapse = ", ")
    created_date <- paste(unique(tbl_df$timestamp_fmt), collapse = ", ")
    rebuild      <- as.integer(any(tbl_df$rebuild_flag == 1, na.rm = TRUE))
    data_srcs <- paste(unique(tbl_df$data_source), collapse = ", ")

    # Build metadata header
    header <- glue::glue(
      "-- ============================================================================",
      "-- Table: {table_name}",
      "-- Columns: {column_count}",
      "-- Data source(s): {data_srcs}",
      "-- Note: {note}",
      "-- Created by: {created_by} on {created_date}",
      if (rebuild == 1) "-- ⚠️ Rebuild triggered by schema changes",
      "-- ============================================================================",
      .sep = "\n"
    )

    # Create column block
    col_block <- glue::glue("  {sql_ident(tbl_df$column_name)} {tbl_df$sql_type} {tbl_df$null_flag}")

    # Add PRIMARY KEY line, if applicable
    pk_cols <- tbl_df %>% dplyr::filter(is_primary_key == "YES") %>% dplyr::pull(column_name)

    pk_line <- if (length(pk_cols) > 0) {
      paste0("  CONSTRAINT ", sql_ident(paste0("pk_", table_name)),
             " PRIMARY KEY (", paste0(sql_ident(pk_cols), collapse = ", "), ")")
    } else NULL

    # Final block
    full_col_block <- paste(c(col_block, pk_line), collapse = ",\n")

    # Final wrapped create statement
    create_stmt <- if (rebuild == 1) {
      glue::glue(
        "DROP TABLE IF EXISTS ", sql_ident(table_name), ";",
        "CREATE TABLE ", sql_ident(table_name), " (",
        "{full_col_block}",
        ");",
        .sep = "\n"
      )
    } else {
      glue::glue(
        "IF NOT EXISTS (",
        "    SELECT 1 FROM sys.objects WHERE name = '{table_name}' AND type = 'U'",
        ")",
        "BEGIN",
        "CREATE TABLE ", sql_ident(table_name), " (",
        "{full_col_block}",
        ");",
        "END",
        .sep = "\n"
      )
    }

    paste(header, create_stmt, "", sep = "\n\n")
  })

  sql_blocks
}
