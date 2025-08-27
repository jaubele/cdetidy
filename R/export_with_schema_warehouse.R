#' Export a dataset to the warehouse with schema logging
#'
#' Safely exports a dataset to the warehouse-ready location and logs its schema metadata to a warehouse-specific SQL schema log.
#' This function wraps both `safe_fwrite_warehouse()` and `sql_schema_log_warehouse()` to ensure standardized output
#' and metadata tracking for the data warehouse pipeline.
#'
#' @param data A data frame to be exported and documented.
#' @param path Optional. The file path where the data will be saved. If `NULL`, the path is generated internally.
#' @param table_name A valid SQL-style name for the exported table.
#' @param data_year A 4-digit string or number indicating the year of the data.
#' @param data_source A short string describing the source of the data (e.g., `"CDE"`, `"SBAC"`).
#' @param data_description A human-readable description of the dataset being exported.
#' @param dim_description Optional. A short description for dimension tables (if applicable).
#' @param user_note A brief note about the export (e.g., filtering, transformations, versioning).
#' @param char_cols Character vector of column names to force as character during export. Defaults to common CDE identifiers.
#' @param compress Logical. If `TRUE`, exports the file as `.csv.gz`. Default is `FALSE`.
#' @param n_check Integer. Number of rows to print during export preview. Default is 6.
#' @param export_log_path File path to the export log CSV. Default is the warehouse export log on the T drive.
#' @param max_char Maximum allowed character length for string columns in the schema log. Default is 255.
#' @param primary_key Optional. A vector of column names representing the table's primary key.
#' @param foreign_keys Optional. A named list representing foreign key relationships.
#' @param canonical_table_id Optional. A unique identifier used for linking across data products.
#' @param dimension_type Optional. A label to describe the type of dimension (e.g., `"student"`, `"school"`).
#'
#' @return Invisibly returns `NULL`. Files are written to disk and schema is logged to the warehouse schema log.
#'
#' @export

export_with_schema_warehouse <- function(data,
                                         path = NULL,
                                         table_name,
                                         data_year,
                                         data_source,
                                         data_description,
                                         dim_description = NULL,
                                         user_note,
                                         char_cols = c("cds", "county_code", "district_code", "school_code"),
                                         compress = FALSE,
                                         n_check = 6,
                                         export_log_path = "T:/Data Warehouse/Warehouse Ready Files/export_log.csv",
                                         max_char = 255,
                                         primary_key = NULL,
                                         foreign_keys = NULL,
                                         canonical_table_id = NULL,
                                         dimension_type = NULL) {

  validate_sql_identifiers(table_name, type = "table")

  validate_sql_identifiers(names(data), type = "column")

  # Run safe_fwrite
  safe_fwrite_warehouse(
    data = data,
    path = path,
    char_cols = char_cols,
    compress = compress,
    n_check = n_check,
    data_year = data_year,
    data_source = data_source,
    data_description = data_description,
    dim_description = dim_description,
    user_note = user_note,
    table_name = table_name,
    log_path = export_log_path,
    canonical_table_id = canonical_table_id,
    dimension_type = dimension_type
  )

  # Run sql_schema_log
  sql_schema_log_warehouse(
    data = data,
    table_name = table_name,
    data_year = data_year,
    data_source = data_source,
    user_note = user_note,
    dim_description = dim_description,
    max_char = max_char,
    primary_key = primary_key,
    foreign_keys = foreign_keys,
    canonical_table_id = canonical_table_id,
    dimension_type = dimension_type
  )

  invisible(NULL)
}
