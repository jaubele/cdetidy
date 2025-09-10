#' Generate and log a SQL schema for a data frame
#'
#' Builds a schema definition table from a data frame by inspecting its column types, nullability,
#' and primary/foreign key indicators. The schema is saved to a central log (`primary_sql_schema_log.csv`)
#' and to an individual table-level schema file.
#'
#' @param data A data frame to analyze.
#' @param table_name The base name of the SQL table.
#' @param data_year A 4-digit year indicating the data vintage.
#' @param data_source A string identifying the data source (must be `"Assessment"`, `"CDE"`, or `"Dashboard"`).
#' @param user_note A short note describing the data (must include `"fact"` or `"dim"`).
#' @param dim_description Optional. A short label describing the dimension content (e.g., `"race_ethnicity"`).
#' @param max_char Maximum length to assign to character columns (default is 255).
#' @param primary_key A character vector of column(s) that make up the primary key.
#' @param foreign_keys A named character vector like `c("column_name" = "ref_table")`.
#' @param canonical_table_id Optional. A unique ID for the table. Defaults to `table_name`.
#' @param dimension_type Optional. One of `"universal"`, `"annualized"`, or `"other"` (used only for dimension tables).
#'
#' @return A tibble containing the schema definition for the input data. The result is also written to:
#' - The central schema log: `T:/Data Warehouse/schema_files/primary_sql_schema_log.csv`
#' - An individual schema file by table: `T:/Data Warehouse/schema_files/<year>/<source>/<fact|dim>/<table>_schema.csv`
#'
#' @details
#' - Infers SQL column types (`VARCHAR`, `INT`, `DECIMAL`, etc.) based on data values.
#' - Enforces uniqueness and completeness of primary key.
#' - Validates format and existence of foreign key mappings.
#' - Updates the central log by replacing existing entries for the same `canonical_table_id`.
#'
#' @export

sql_schema_log_warehouse <- function(data,
                                     table_name,
                                     data_year,
                                     data_source,
                                     user_note = NA,
                                     dim_description = NULL,
                                     max_char = 255,
                                     primary_key = NULL,
                                     foreign_keys = NULL,   # now supports named vector OR list-of-lists
                                     canonical_table_id = NULL,
                                     dimension_type = NULL) {

  # -----------------------------
  # Validate data_source
  # -----------------------------
  valid_sources <- c("Assessment", "CDE", "Dashboard")
  if (is.null(data_source) || !(tolower(data_source) %in% tolower(valid_sources))) {
    stop("❌ `data_source` must be one of: ", paste(valid_sources, collapse = ", "))
  }
  data_source_label <- valid_sources[match(tolower(data_source), tolower(valid_sources))]
  data_source_dir <- tolower(data_source_label)

  # -----------------------------
  # Validate user_note includes fact|dim and derive table_type
  # -----------------------------
  if (is.null(user_note) || !grepl("\\b(fact|dim)\\b", user_note, ignore.case = TRUE)) {
    stop("❌ Your `user_note` should clearly include either 'fact' or 'dim'.")
  }
  table_type <- tolower(stringr::str_extract(user_note, "\\b(fact|dim)\\b"))

  # -----------------------------
  # Clean dim_description (optional)
  # -----------------------------
  if (!is.null(dim_description)) {
    dim_description <- janitor::make_clean_names(dim_description)
  }

  # -----------------------------
  # Coerce to data.table and validate primary key
  # -----------------------------
  data <- data.table::as.data.table(data)

  if (is.null(primary_key)) {
    stop("❌ You should supply a primary key for this table using `primary_key`.")
  }
  
  if (!all(primary_key %in% names(data))) {
    stop("❌ The specified primary key does not exist in the dataset: ",
         paste(setdiff(primary_key, names(data)), collapse = ", "))
  }
  
  # Uniqueness checks
  # For any length(primary_key) >= 1
  if (data.table::uniqueN(data, by = primary_key) < nrow(data)) {
    dups <- data[, .N, by = primary_key][N > 1][order(-N)]
    stop(
      "❌ Duplicate primary key values detected (showing top few):\n",
      paste(utils::capture.output(print(head(dups, 10))), collapse = "\n")
    )
  }
  
  # Missing checks
  if (anyNA(data[, .SD, .SDcols = primary_key])) {
    stop("❌ One or more of the primary key columns (",
         paste(primary_key, collapse = ", "), ") contain missing values.")
  }

  # -----------------------------
  # Foreign key normalization (backward-compatible)
  #   - Accepts named character vector: c(col = "ref_table")
  #   - Or list-of-lists: list(list(columns=c(...), ref_table="..."), ...)
  # -----------------------------
  normalize_foreign_keys <- function(fk, data_cols) {
    if (is.null(fk)) return(list())

    # Old style: named character vector
    if (is.character(fk) && !is.null(names(fk))) {
      fk <- purrr::imap(fk, ~ list(columns = .y, ref_table = .x, ref_column = .y))
    }

    if (!is.list(fk)) {
      stop("❌ `foreign_keys` should be a named character vector or a list of lists with `columns` and `ref_table` (and optionally `ref_column).")
    }

    # Validate each entry
    fk <- lapply(fk, function(x) {
      if (!all(c("columns", "ref_table") %in% names(x))) {
        stop("❌ Each foreign key should have `columns` and `ref_table` fields.")
      }

      cols <- as.character(x$columns)
      ref_cols <- if(!is.null(x$ref_column)) as.character(x$ref_column) else cols

      if(length(cols) != length(ref_cols)) {
        stop("❌ In foreign_keys: `columns` and `ref_column` must have the same length.")
      }

      if (!all(cols %in% data_cols)) {
        stop("❌ Foreign key columns not found in the dataset: ",
             paste(setdiff(cols, data_cols), collapse = ", "))
      }
      list(columns = cols,
           ref_table = as.character(x$ref_table)[1],
           ref_column = ref_cols)
    })

    fk
  }

  foreign_keys <- normalize_foreign_keys(foreign_keys, names(data))

  # -----------------------------
  # Dimension type validation (optional)
  # -----------------------------
  canonical_table_id <- canonical_table_id %||% table_name

  dimension_type <- dimension_type %||% NA

  valid_dimension_types <- c("universal", "annualized", "other")
  if (!is.null(dimension_type) && !is.na(dimension_type)) {
    if (!tolower(dimension_type) %in% valid_dimension_types) {
      stop("❌ `dimension_type` should be one of: ",
           paste(valid_dimension_types, collapse = ", "))
    }
    dimension_type <- tolower(dimension_type)
  }

  # -----------------------------
  # Helper: infer SQL type
  # -----------------------------
  measure_sql_type <- function(x) {
    if (is.character(x)) {
      max_len <- suppressWarnings(max(nchar(x[!is.na(x)]), na.rm = TRUE))
      if (!is.finite(max_len)) max_len <- max_char
      return(paste0("VARCHAR(", min(max_len, max_char), ")"))
    } else if (is.integer(x)) {
      return("INT")
    } else if (is.numeric(x)) {
      vals <- stats::na.omit(x)
      if (length(vals) == 0) return("DECIMAL(10, 2)")

      # pretty-format to count digits reliably (no scientific notation)
      f <- format(vals, scientific = FALSE, trim = TRUE)
      whole_digits <- nchar(gsub("\\..*$", "", f))               # digits left of decimal
      frac_digits  <- ifelse(grepl("\\.", f),
                             nchar(sub("^[^.]*\\.", "", f)), 0)  # digits right of decimal
      p <- suppressWarnings(max(whole_digits + frac_digits, na.rm = TRUE))
      s <- suppressWarnings(max(frac_digits, na.rm = TRUE))
      if (!is.finite(p)) p <- 10
      if (!is.finite(s)) s <- 4
      # If no fractional digits detected, use INT
      if (s == 0) return("INT")
      return(paste0("DECIMAL(", p, ", ", s, ")"))
    }
    else if (is.logical(x)) {
      return("BIT")
    } else {
      return("NVARCHAR(MAX)")
    }
  }

  current_cols <- names(data)

  # -----------------------------
  # Build enhanced final table name
  # -----------------------------
  final_table_name <- paste0(
    table_name,
    if (!is.null(dim_description)) paste0("_", dim_description),
    "_", table_type
  )

  # -----------------------------
  # Build FK column map for schema logging
  # -----------------------------
  fk_column_map <- if (length(foreign_keys) == 0) {
    tibble::tibble(column = character(), ref_column = character(), ref_table = character(), fk_group = character(), ref_group = character())
  } else {
    purrr::map_dfr(foreign_keys, function(fk) {
      tibble::tibble(
        column     = fk$columns,
        ref_column = fk$ref_column,
        ref_table  = fk$ref_table,
        fk_group   = paste(fk$columns, collapse = " + "),
        ref_group  = paste(fk$ref_column, collapse = " + ")
      )
    })
  }

  # -----------------------------
  # Build schema tibble
  # -----------------------------
  # Make fk_column_map unique just in case
  fk_column_map <- dplyr::distinct(fk_column_map)

  schema <- tibble::tibble(
    table_name      = final_table_name,
    column_name     = current_cols,
    sql_type        = purrr::map_chr(data, measure_sql_type),
    null_flag       = purrr::map_chr(data, ~ ifelse(any(is.na(.x)), "NULL", "NOT NULL")),
    is_primary_key  = ifelse(current_cols %in% primary_key, "YES", "NO"),
    is_foreign_key  = ifelse(current_cols %in% fk_column_map$column, "YES", "NO"),

    # ✅ collapse multiple refs for a column into one string
    foreign_key_ref = purrr::map_chr(current_cols, function(col) {
      rows <- dplyr::filter(fk_column_map, column == col)
      if (nrow(rows) > 0) {
        paste0(paste0(rows$ref_table, "(", rows$ref_column, ")"), collapse = " | ")
      } else NA_character_
    }),

    # ✅ likewise, collapse multiple groups if the column participates in multiple composites
    foreign_key_group = purrr::map_chr(current_cols, function(col) {
      rows <- dplyr::filter(fk_column_map, column == col)
      if (nrow(rows) > 0) paste(unique(rows$fk_group), collapse = " | ") else NA_character_
    }),

    primary_key_group = ifelse(
      current_cols %in% primary_key,
      paste(primary_key, collapse = " + "),
      NA_character_
    ),
    ref_key_group = purrr::map_chr(current_cols, function(col) {
      rows <- dplyr::filter(fk_column_map, column == col)
      if (nrow(rows) > 0) paste(unique(rows$ref_group), collapse = " | ") else NA_character_
    }),
    dimension_type     = dimension_type,
    data_year          = data_year,
    data_source        = data_source_label,
    dim_description    = dim_description %||% NA,
    canonical_table_id = canonical_table_id,
    user_note          = user_note %||% NA,
    timestamp          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user               = Sys.info()[["user"]]
  ) %>%
    dplyr::select(
      table_name, column_name, sql_type, null_flag,
      is_primary_key, is_foreign_key, foreign_key_ref, foreign_key_group,
      primary_key_group, ref_key_group, dimension_type, data_year, data_source,
      canonical_table_id, dim_description, user_note, timestamp, user
    )

  # -----------------------------
  # Persist schema logs
  # -----------------------------
  base_dir <- "T:/Data Warehouse/Warehouse Ready Files"
  schema_log_dir <- file.path(base_dir, "schema_files")
  primary_log_path <- file.path(schema_log_dir, "primary_sql_schema_log.csv")

  if (!dir.exists(dirname(primary_log_path))) {
    dir.create(dirname(primary_log_path), recursive = TRUE)
  }

  if (file.exists(primary_log_path)) {
    existing_log <- readr::read_csv(primary_log_path, show_col_types = FALSE)

    existing_log$timestamp <- as.character(existing_log$timestamp)

    # Replace rows for this canonical_table_id (keeps latest single-table snapshot)
    schema <- dplyr::bind_rows(
      dplyr::filter(existing_log, canonical_table_id != !!canonical_table_id),
      schema
    )
  }

  # Always reset; changes flagged separately later
  schema$rebuild_flag <- 0

  readr::write_csv(schema, primary_log_path)
  message("🧾 Primary schema log updated at: ", primary_log_path)

  # Per-table schema file
  individual_path <- file.path(
    schema_log_dir,
    as.character(data_year),
    data_source_dir,
    table_type,
    paste0(final_table_name, "_schema.csv")
  )
  if (!dir.exists(dirname(individual_path))) {
    dir.create(dirname(individual_path), recursive = TRUE)
  }

  current_schema <- dplyr::filter(schema, table_name == !!final_table_name)
  readr::write_csv(current_schema, individual_path)
  message("📄 Table-level schema saved at: ", individual_path)

  invisible(schema)
}
