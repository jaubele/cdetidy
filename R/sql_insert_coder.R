#' Generate SQL BULK INSERT statements for warehouse flat files
#'
#' Creates a character vector of `BULK INSERT` SQL statements for loading flat files into SQL Server tables.
#' Uses metadata from a flagged schema (e.g., from `sql_schema_log()` or `flag_schema_changes()`)
#' to determine table names and file paths, and emits standardized SQL for bulk-loading.
#'
#' @param schema_flagged A data frame containing schema metadata, including columns:
#' - `table_name`: Name of the table to load
#' - `data_source`: Data source type (e.g., `"CDE"`, `"Dashboard"`, `"Assessment"`)
#' - `dimension_type`: Optional type (`"universal"` or other) to determine subfolder structure
#' @param data_base_dir Root directory where data files are stored. Default is `"T:/Data Warehouse/Warehouse Ready Files"`.
#' @param combined_folder The name of the subdirectory (e.g., `"2019_2024"`) that contains the flat files. Required.
#' @param universal_folder Folder name for universal dimensions. Default is `"universal"`.
#'
#' @return A character vector of `BULK INSERT` SQL statements, one per table. Each statement includes:
#' - Path to the `.csv` file
#' - Error log configuration
#' - A statement to increment `@__bulk_executed`
#'
#' @details
#' - File paths are validated, and missing files cause the function to fail early with a helpful message.
#' - Uses `sql_ident()` to quote table names and escapes single quotes in file paths.
#' - Assumes CSV files are encoded using Windows ANSI (`CODEPAGE = 'ACP'`) and start on line 2.
#'
#' @export

sql_insert_coder <- function(schema_flagged, data_base_dir = "T:/Data Warehouse/Warehouse Ready Files",
                             combined_folder = NULL, universal_folder = "universal") {

  if (is.null(combined_folder)) {
    stop("❌ Please provide the name of the combined folder (e.g., '2019_2024').")
  }

  # helpers: SQL string literal escape & file-name safe table
  esc_sql_str <- function(x) gsub("'", "''", x, fixed = TRUE)
  file_safe   <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)

  # Compute subdir once, then de-dupe by (table_name, subdir)
  insert_rows <- schema_flagged %>%
    dplyr::mutate(
      subdir = dplyr::if_else(
        !is.na(dimension_type) & tolower(dimension_type) == "universal",
        universal_folder,
        tolower(data_source)
      )
    ) %>%
    dplyr::distinct(table_name, subdir) %>%
    dplyr::arrange(table_name, subdir)

  if (nrow(insert_rows) == 0) {
    stop("⚠️ No tables found in schema_flagged.")
  }

  blocks <- purrr::pmap_chr(insert_rows, function(table_name, subdir, ...) {
    csv_path <- file.path(
      data_base_dir, as.character(combined_folder), "Data Files",
      subdir, paste0(table_name, ".csv")
    )

    if (!file.exists(csv_path)) {
      stop("⚠️ Flat file not found for table: ", table_name, "\n  → Expected at: ", csv_path)
    }

    # use sql_ident() for the table name; escape path and safe name for error log
    table_ident <- sql_ident(table_name)
    csv_sql     <- esc_sql_str(csv_path)
    log_name    <- paste0("bulk_", file_safe(table_name), ".log")

    glue::glue(
      "-- Insert new rows into {table_name}",
      "BULK INSERT {table_ident}",
      "FROM N'{csv_sql}'",
      "WITH (",
      "  FORMAT = 'CSV',",
      "  FIRSTROW = 2,",
      "  CODEPAGE = 'ACP',",
      "  TABLOCK,",
      "  MAXERRORS = 0,",
      "  ERRORFILE = N'$(ErrorDir)\\{log_name}'",
      ");",
      "SET @__bulk_executed = @__bulk_executed + 1;",
      .sep = "\n"
    )
  }
  )

  blocks

}
