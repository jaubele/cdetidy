#' Archive the primary SQL schema log for a given year
#'
#' Copies the current `primary_sql_schema_log.csv` from the live folder
#' to an archive folder with the year included in the filename. Prevents
#' overwriting unless explicitly allowed.
#'
#' @param year A 4-digit year as a numeric or character string. Used to name the archived file.
#' @param overwrite Logical. If TRUE, allows overwriting an existing archive file. Default is FALSE.
#'
#' @return (Invisibly) the path to the archived schema log file as a character string.
#'
#' @export


archive_schema_log <- function(year, overwrite = FALSE) {
  if (is.numeric(year)) year <- as.character(year)
  stopifnot(!is.null(year), nchar(year) == 4)

  live_path   <- "T:/Data Warehouse/Warehouse Ready Files/schema_files/primary_sql_schema_log.csv"
  archive_dir <- "T:/Data Warehouse/Warehouse Ready Files/schema_files/Archive"
  archive_path <- file.path(archive_dir, paste0("primary_sql_schema_log_", year, ".csv"))

  if (!file.exists(live_path)) {
    stop("❌ Live primary_sql_schema_log.csv does not exist at: ", live_path)
  }
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  if (file.exists(archive_path) && !overwrite) {
    stop("❌ Archive already exists: ", archive_path,
         "\n   Pass overwrite = TRUE to replace it.")
  }

  # Read/write once with readr, no extra timestamp rows
  schema <- readr::read_csv(live_path, show_col_types = FALSE)
  readr::write_csv(schema, archive_path, na = "")

  cat("✅ Schema log archived to: ", archive_path, "\n")
  invisible(archive_path)
}
