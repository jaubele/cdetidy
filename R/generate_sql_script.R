#' Generate a T-SQL script for loading warehouse tables
#'
#' Builds a full SQL script to create tables, insert data, and apply foreign key constraints
#' based on a flagged schema log and a prior-year comparison. The script includes metadata tracking,
#' expected object checks, and runtime diagnostics for bulk loading into a SQL Server warehouse.
#'
#' @param prior_year A 4-digit year (numeric or character) used to locate the archived schema log for comparison.
#' @param combined_folder A string that names the folder containing flat files to be loaded (e.g., `"2019_2024"`).
#' @param data_source A string indicating the source category (`"assessment"`, `"cde"`, or `"dashboard"`).
#' @param schema_log_path Path to the folder containing schema logs. Default is the shared warehouse T drive path.
#'
#' @return Invisibly returns the full SQL script as a single string. Also writes the script to a `.sql` file
#' in the `schema_log_path` directory with a filename formatted as `<source>_data_warehouse_<combined_folder>.sql`.
#'
#' @details
#' - Automatically flags schema differences using `flag_schema_changes()` and saves a frozen copy of the schema log.
#' - Assembles the full script using `sql_table_coder()`, `sql_insert_coder()`, and `sql_foreign_key_coder()`.
#' - Automatically builds declarations for expected tables and FKs and prints progress and summary info.
#' - Generates SQL for error detection, transaction control, and post-run diagnostics.
#'
#' @export

generate_sql_script <- function(
    prior_year,             # e.g., 2024 (for rebuild comparison)
    combined_folder,         # e.g., "2019_2024" (for flat file locations)
    data_source,
    schema_log_path = "T:/Data Warehouse/Warehouse Ready Files/schema_files")

{
  # --- Imports kept local for clarity and self-containment
  library(dplyr)
  library(glue)
  library(readr)
  library(purrr)
  library(tibble)

  # --- validate single source
  valid_keys <- c("assessment","cde","dashboard")
  if (length(data_source) != 1) {
    stop("❌ Provide exactly one data_source (assessment, cde, or dashboard).")
  }

  ds <- tolower(as.character(data_source))

  if (!ds %in% valid_keys) {
    stop("❌ data_source must be one of: assessment, cde, dashboard.")
  }

  # --- read the primary log once
  primary_log_file   <- file.path(schema_log_path, "primary_sql_schema_log.csv")
  schema_log_all <- readr::read_csv(primary_log_file, show_col_types = FALSE)

  # filter to the one source (+ universal via filter function)
  schema_log <- filter_schema_sources(schema_log_all, data_source = ds)

  # compare vs archive and flag
  schema_flagged <- flag_schema_changes(schema_log, prior_year)

  # derive curr_year (prefer data if present) -
  curr_year <- {
    yrs <- suppressWarnings(as.integer(schema_flagged$data_year))
    if (length(yrs) && any(!is.na(yrs))) max(yrs, na.rm = TRUE) else as.integer(prior_year) + 1L
  }

  # persist a per-source frozen schema log
  generated_log_file <- file.path(
    schema_log_path,
    sprintf("primary_sql_schema_log_%s_%s.csv", ds, curr_year)
  )
  readr::write_csv(schema_flagged, generated_log_file)
  cat("📄 Updated flag schema log saved at: ", generated_log_file, "\n", sep = "")

  # Step 4: Build blocks
  table_blocks <- sql_table_coder(schema_flagged)
  insert_blocks <- sql_insert_coder(schema_flagged, combined_folder = combined_folder)
  fk_blocks <- sql_foreign_key_coder(schema_flagged)

  # Step 5. Find and record expected counts and names
  n_create <- length(table_blocks)
  n_insert <- length(insert_blocks)
  tbl_names <- sort(unique(schema_flagged$table_name))

  # Build expected FK constraint names exactly like sql_foreign_key_coder()
  fk_names <- sort(fk_constraint_names(schema_flagged))
  n_fk     <- length(fk_names)

  # Build SQL VALUES lists for expected sets
  # (N'...') is Unicode-safe; escape any single quotes if present
  esc <- function(x) gsub("'", "''", x, fixed = TRUE)
  expected_tables_sql <- if (length(tbl_names)) {
    paste0("INSERT INTO @ExpectedTables(name) VALUES ",
           paste0("(N'", esc(tbl_names), "')", collapse = ", "), ";")
  } else "/* No tables expected */"

  expected_fks_sql <- if (length(fk_names)) {
    paste0("INSERT INTO @ExpectedFKs(name) VALUES ",
           paste0("(N'", esc(fk_names), "')", collapse = ", "), ";")
  } else "/* No FKs expected */"

  has_universal <- any(tolower(schema_flagged$dimension_type) == "universal", na.rm = TRUE)
  ds_title <- c(assessment = "Assessment", cde = "CDE", dashboard = "Dashboard")[ds]
  src_log <- basename(generated_log_file)

  if (length(fk_blocks) != length(fk_names)) {
    warning(glue::glue("FK count mismatch: blocks={length(fk_blocks)}, names={length(fk_names)}"))
  }

  # Step 6: Compose output in "post-load FKs" order
  header_note <- glue::glue("-- =============================================================================
-- AUTO-GENERATED SQL CREATE + INSERT SCRIPT
-- =============================================================================
-- Source schema log (frozen for this run): {src_log}
-- Data source: {ds_title}{if (has_universal) ' + universal' else ''}
-- Archive compare year: {prior_year}
-- Current data year: {curr_year}
-- Generated at: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}
-- Mode: Smooth bulk load (FKs AFTER inserts; integrity checked at constraint add)
-- =============================================================================
-- Optional: Set your target database
-- USE [YourDatabaseNameHere];
-- GO
-- =============================================================================
-- Configure error log directory for BULK INSERT (SSMS must run in SQLCMD Mode)
:setvar ErrorDir \"C:\\DW\\error_log\"

DECLARE @__run_started_at DATETIME2(3) = SYSDATETIME();
PRINT N'=== DW load started at ' + CONVERT(nvarchar(30), @__run_started_at, 126) + ' ===';
PRINT N'Using error log directory: $(ErrorDir)';

-- ---------- Expected summary (from generator) ----------
DECLARE @__create_expected INT = {n_create};
DECLARE @__insert_expected INT = {n_insert};
DECLARE @__fk_expected     INT = {n_fk};

PRINT N'Expected blocks — CREATE=' + CAST(@__create_expected AS nvarchar(20))
    + N', INSERT=' + CAST(@__insert_expected AS nvarchar(20))
    + N', FKs='    + CAST(@__fk_expected     AS nvarchar(20));

-- Expected name sets
DECLARE @ExpectedTables TABLE (name sysname PRIMARY KEY);
{expected_tables_sql}

DECLARE @ExpectedFKs TABLE (name sysname PRIMARY KEY);
{expected_fks_sql}

-- Runtime counters
DECLARE @__bulk_expected INT = @__insert_expected;
DECLARE @__bulk_executed INT = 0;

\n\n")

  full_script <- glue::glue("
{header_note}

SET XACT_ABORT ON;
SET NOCOUNT ON;

BEGIN TRY
    BEGIN TRAN;

    -- === Create empty tables ===
{paste(table_blocks, collapse = '\n\n')}

    -- === Load data (no FK checks yet) ===
{paste(insert_blocks, collapse = '\n\n')}

    -- === Add FKs (validates loaded data) ===
{paste(fk_blocks, collapse = '\n\n')}

    COMMIT TRAN;

    -- (post-run counters & prints)
    DECLARE @__created_tables INT = (
        SELECT COUNT(*) FROM sys.objects
        WHERE type = 'U' AND name IN (SELECT name FROM @ExpectedTables)
    );
    DECLARE @__created_fks INT = (
        SELECT COUNT(*) FROM sys.foreign_keys
        WHERE name IN (SELECT name FROM @ExpectedFKs)
    );

    DECLARE @__run_finished_at DATETIME2(3) = SYSDATETIME();
    PRINT N'=== DW load finished at ' + CONVERT(nvarchar(30), @__run_finished_at, 126) + ' ===';
    PRINT N'Elapsed (seconds): ' + CONVERT(nvarchar(20), DATEDIFF(SECOND, @__run_started_at, @__run_finished_at));

    PRINT N'Tables created =' + CAST(@__created_tables AS nvarchar(20))
        + N', INSERT executed =' + CAST(@__bulk_executed AS nvarchar(20))
        + N', FKs created =' + CAST(@__created_fks AS nvarchar(20));

    IF @__created_tables <> @__create_expected
    BEGIN
        PRINT N'⚠️ Tables expected but not present:';
        SELECT name AS MissingTable FROM @ExpectedTables t
        WHERE NOT EXISTS (SELECT 1 FROM sys.objects o WHERE o.type = 'U' AND o.name = t.name);
    END

    IF @__created_fks <> @__fk_expected
    BEGIN
        PRINT N'⚠️ FK constraints expected but not present:';
        SELECT name AS MissingFK FROM @ExpectedFKs f
        WHERE NOT EXISTS (SELECT 1 FROM sys.foreign_keys k WHERE k.name = f.name);
    END

    IF @__bulk_executed <> @__bulk_expected
    BEGIN
        PRINT N'⚠️ Not all BULK INSERT statements executed successfully.';
        PRINT N'   Expected INSERTs: ' + CAST(@__bulk_expected AS nvarchar(20))
            + N', Executed: ' + CAST(@__bulk_executed AS nvarchar(20));
    END

END TRY
BEGIN CATCH
    IF @@TRANCOUNT > 0 ROLLBACK TRAN;
    DECLARE @ErrMsg NVARCHAR(4000) = ERROR_MESSAGE();
    DECLARE @ErrSev INT = ERROR_SEVERITY();
    DECLARE @ErrSta INT = ERROR_STATE();
    THROW 50000, @ErrMsg, 1;
END CATCH
")

  output_path <- file.path(schema_log_path,
                           sprintf("%s_data_warehouse_%s.sql", ds, combined_folder))

  readr::write_file(full_script, output_path)
  cli::cli_alert_success(glue::glue("✅ SQL script written to: {output_path}"))
  invisible(full_script)
}
