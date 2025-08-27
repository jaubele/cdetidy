sql_schema_log <- function(data,
                           table_name,
                           data_year,
                           data_source,
                           user_note = NA,
                           dim_description = NULL,
                           max_char = 255,
                           primary_key = NULL,
                           foreign_keys = NULL,
                           canonical_table_id = NULL,
                           dimension_type = NULL) {

  # Validate required data_source
  valid_sources <- c("Assessment", "CDE", "Dashboard")
  if (is.null(data_source) || !(tolower(data_source) %in% tolower(valid_sources))) {
    stop("❌ `data_source` must be one of: ", paste(valid_sources, collapse = ", "))
  }

  data_source_label <- valid_sources[match(tolower(data_source), tolower(valid_sources))]
  data_source_dir <- tolower(data_source_label)

  # Check for fact or dim in user_note
  if (is.null(user_note) || !grepl("\\b(fact|dim)\\b", user_note, ignore.case = TRUE)) {
    stop("❌ Your `user_note` must clearly include either 'fact' or 'dim'.")
  }
  table_type <- tolower(stringr::str_extract(user_note, "\\b(fact|dim)\\b"))

  # Clean dim_description
  if (!is.null(dim_description)) {
    dim_description <- janitor::make_clean_names(dim_description)
  }

  # --- Validate primary key input ---
  data <- data.table::as.data.table(data)

  if (is.null(primary_key)) {
    stop("❌ You must supply a primary key for this table using primary_key.")
  }

  if (!all(primary_key %in% names(data))) {
    stop("❌ The specified primary key does not exist in the dataset: ",
         paste(setdiff(primary_key, names(data)), collapse = ", "))
  }

  # Optional but powerful: warn if not unique
  if (length(primary_key) == 1 && anyDuplicated(data[[primary_key]]) > 0) {
    stop("❌   The primary key column '", primary_key, "' contains duplicates.")
  }

  if (length(primary_key) > 1 && anyDuplicated(data[, ..primary_key, drop = FALSE]) > 0) {
    stop("❌ ️ The composite primary key (", paste(primary_key, collapse = ", "), ") contains duplicates.")
  }

  if (any(is.na(dplyr::select(data, all_of(primary_key))))) {
    stop("❌ ️ One or more of the primary key columns (",
         paste(primary_key, collapse = ", "), ") contain missing values.")
  }

  # --- Foreign key validation ---
  if (!is.null(foreign_keys)) {
    if (is.null(names(foreign_keys)) || any(names(foreign_keys) == "")) {
      stop("❌ foreign_keys must be a named character vector: c(col = 'ref_table')")
    }
    if (!all(names(foreign_keys) %in% names(data))) {
      stop("❌ Some foreign key columns are not found in the dataset: ",
           paste(setdiff(names(foreign_keys), names(data)), collapse = ", "))
    }
  }

  # Clean dim_description
  if (!is.null(dim_description)) {
    dim_description <- janitor::make_clean_names(dim_description)
  }

  # Build enhanced table name
  final_table_name <- paste0(
    table_name,
    if (!is.null(dim_description)) paste0("_", dim_description),
    "_", table_type
  )

  # Fallback if NULL
  user_note <- user_note %||% NA

  canonical_table_id <- canonical_table_id %||% table_name

  dimension_type <- dimension_type %||% NA

  valid_dimension_types <- c("universal", "annualized", "other")
  if (!is.null(dimension_type) && !is.na(dimension_type)) {
    if (!tolower(dimension_type) %in% valid_dimension_types) {
      stop("❌ `dimension_type` must be one of: ",
           paste(valid_dimension_types, collapse = ", "))
    }
    dimension_type <- tolower(dimension_type)
  }

  # Helper: Measure SQL type
  measure_sql_type <- function(x) {
    if (is.character(x)) {
      max_len <- max(nchar(x[!is.na(x)]), na.rm = TRUE)
      return(paste0("VARCHAR(", min(max_len, max_char), ")"))
    }
    else if (is.integer(x)) {
      max_digits <- max(nchar(abs(x[!is.na(x)])), na.rm = TRUE)
      return(paste0("INT(", max_digits, ")"))
    }
    else if (is.numeric(x)) {
      x <- na.omit(x)
      if (length(x) == 0) return("DECIMAL(10, 2)")

      formatted <- format(x, scientific = FALSE)
      whole_digits <- nchar(gsub("\\..*$", "", formatted))
      frac_digits <- nchar(gsub("^[^.]*\\.", "", formatted))
      frac_digits[!grepl("\\.", formatted)] <- 0

      p <- max(whole_digits + frac_digits, na.rm = TRUE)
      s <- max(frac_digits, na.rm = TRUE)
      return(paste0("DECIMAL(", p, ", ", s, ")"))
    }
    else if (is.logical(x)) {
      return("BOOLEAN")
    }
    else {
      return("TEXT")
    }
  }

  current_cols <- names(data)  # define just above the tibble

  # Build schema tibble
  schema <- tibble::tibble(
    table_name = final_table_name,
    column_name = names(data),
    sql_type = purrr::map_chr(data, measure_sql_type),
    null_flag = purrr::map_chr(data, ~ ifelse(any(is.na(.x)), "NULL", "NOT NULL")),
    is_primary_key = ifelse(current_cols %in% primary_key, "YES", "NO"),
    is_foreign_key = ifelse(current_cols %in% names(foreign_keys), "YES", "NO"),
    foreign_key_ref = ifelse(
      current_cols %in% names(foreign_keys),
      paste0(foreign_keys[current_cols], "(", current_cols, ")"),
      NA_character_),
    primary_key_group = ifelse(
      current_cols %in% primary_key,
      paste(primary_key, collapse = " + "),
      NA_character_),
    dimension_type = dimension_type,
    data_year = data_year,
    data_source = data_source_label,
    dim_description = dim_description %||% NA,
    canonical_table_id = canonical_table_id,
    user_note = user_note,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user = Sys.info()[["user"]]
  ) %>%
    dplyr::select(table_name, column_name, sql_type, null_flag,
                  is_primary_key, is_foreign_key, foreign_key_ref,
                  primary_key_group, dimension_type, data_year, data_source,
                  canonical_table_id, dim_description, user_note,
                  timestamp, user)

  # Define shared base directory for schema files
  base_dir <- "T:/Data Warehouse"

  # Define subpaths using base_dir
  schema_log_dir <- file.path(base_dir, "schema_files")
  primary_log_path <- file.path(schema_log_dir, "primary_sql_schema_log.csv")

  # Save to primary log
  if (!dir.exists(dirname(primary_log_path))) {
    dir.create(dirname(primary_log_path), recursive = TRUE)
  }

  if (file.exists(primary_log_path)) {
    existing_log <- readr::read_csv(primary_log_path, show_col_types = FALSE)

    # Convert timestamp to character for consistent binding
    existing_log$timestamp <- as.character(existing_log$timestamp)

    schema <- dplyr::bind_rows(
      dplyr::filter(existing_log, canonical_table_id != !!canonical_table_id),
      schema
    )
  }

  readr::write_csv(schema, primary_log_path)
  message("🧾 Primary schema log updated at: ", primary_log_path)

  # Save individual schema file
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
  # Filter just the current table’s schema
  current_schema <- dplyr::filter(schema, table_name == !!final_table_name)
  readr::write_csv(current_schema, individual_path)
  message("📄 Table-level schema saved at: ", individual_path)

  invisible(schema)
}
