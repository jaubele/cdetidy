#' Safely write a dataset to CSV with metadata logging
#'
#' Writes a data frame to a standardized T drive location (or user-defined path), enforces character
#' types on specific ID columns, applies optional compression, and logs metadata to a central export log.
#' Supports both fact and dimension tables and validates metadata inputs before writing.
#'
#' @param data A data frame to export.
#' @param path Optional. Full file path to write the CSV. If `NULL`, a path is generated automatically based on metadata.
#' @param char_cols A character vector of column names to convert to character before export. Default is `c("cds", "county_code", "district_code", "school_code")`.
#' @param compress Logical. If `TRUE`, appends `.gz` to the filename and compresses the output. Default is `FALSE`.
#' @param n_check Integer. Number of rows to preview after writing. Default is 6.
#' @param log_metadata A named list containing metadata fields, including `data_year`, `data_source`, `data_description`, and `user_note`. If `NULL`, the function uses the corresponding individual arguments.
#' @param data_year The year the data represents. Required if `log_metadata` is not supplied.
<<<<<<< HEAD
#' @param data_source A short label identifying the source of the data (e.g., `"CDE"`, `"Dashboard"`).
#' @param data_type Short label used to choose the subfolder under the data source (e.g., `"Absenteeism"` under `"CDE"`). Required if path is NULL. Case-insensitive; common synonyms accepted.
=======
#' @param data_source A short label identifying the source of the data (e.g., `"CDE"`, `"Dashboard"`, `"Assessment"`).
#' @param data_type A short label identifying what type of data you are writing out (e.g., "`CAST`", "`SBAC`", "`Absenteeism`", dims)
>>>>>>> 32799a6e06203125ff4bef9f816701abc3c65dd9
#' @param data_description A short description of the dataset (e.g., `"Chronic absenteeism rates by subgroup"`).
#' @param user_note A note describing the nature of the export. Must include `"fact"` or `"dim"` to indicate table type.
#' @param table_name The base name of the output table.
#' @param dim_description Optional. A short label used to describe the dimension (e.g., `"race_ethnicity"`). Appended to the file name.
#' @param log_path The path to the export log CSV. Default is `"export_log.csv"`.
#' @param canonical_table_id Optional. A unique identifier for the exported table. Defaults to `table_name` if `NULL`.
#' @param dimension_type Optional. One of `"universal"`, `"annualized"`, or `"other"`. Applies to dimension tables.
#'
#' @return Invisibly returns `NULL`. The function writes the data to disk and logs metadata to a central log file.
#'
#' @details
#' - Validates required metadata fields and data source types.
#' - Pads dimension codes as needed and enforces character types on key fields.
#' - If `log_path` exists, overwrites log entry for the same `canonical_table_id`, otherwise appends a new row.
#' - Automatically creates directories if needed and checks file size after writing.
#'
#' @export

safe_fwrite <- function(
    data, path = NULL,
    char_cols = c("cds","county_code","district_code","school_code"),
    compress = FALSE,
    n_check = 6,
    log_metadata = NULL,
    data_year = NULL,
    data_source = NULL,
    data_type = NULL,         # validated below
    data_description = NA,
    user_note = NA,
    table_name = NULL,
    dim_description = NULL,
    log_path = "export_log.csv",
    canonical_table_id = NULL,
<<<<<<< HEAD
    dimension_type = NULL) {

  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

  norm_token <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))

=======
    dimension_type = NULL
) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
  norm_token <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))
>>>>>>> 32799a6e06203125ff4bef9f816701abc3c65dd9
  title_underscore <- function(x) gsub("\\s+", "_", tools::toTitleCase(gsub("_", " ", x)))

  # --- catalogs & resolver ---
  catalog <- list(
    Assessment = list(
      values = c("SBAC","CAST","ELPAC", "dim"),
      syns = list(
        SBAC  = c("sbac","smarter","smarterbalanced"),
        CAST  = c("cast","science"),
        ELPAC = c("elpac","englishlanguageproficiency","elpa"),
        dim = c("dim", "dimension")
      )
    ),
    CDE = list(
      values = c("Absenteeism","Enrollment","Discipline","EL","Grad_Dropout","Post_Secondary", "dim"),
      syns = list(
        Absenteeism    = c("absenteeism","chronic","chronicabsenteeism"),
        Enrollment     = c("enrollment","enrol"),
        Discipline     = c("discipline","suspension","suspensions"),
        EL             = c("el","englishlearner","ell","englishlearners"),
        Grad_Dropout   = c("graddropout","graduation","grad","dropout","cohort"),
        Post_Secondary = c("postsecondary","post_secondary","collegecareer","cci","collegeandcareer"),
        dim = c("dim", "dimension")
      )
    ),
    Dashboard = list(
      values = c("Achievement", "Engagement", "Climate", "Broad_Course", "Info_Only", "dim"),
      syns = list(
        Achievement = c("ela", "math", "elpi", "academics"),
        Engagement = c("grad", "grad_rate", "gr", "absenteeism", "ca", "chronic"),
        Climate = c("suspension","sus", "susp"),
        Broad_Course = c("cci", "college_and_career", "college", "career"),
        Info_Only = c("science", "sci", "growth_rate", "growth"),
        dim = c("dim", "dimension"))
    )
  )

  resolve_type <- function(ds_label, dt_input) {
    catg <- catalog[[ds_label]]
    if (is.null(catg)) stop("❌ Unsupported data_source catalog: ", ds_label)
    tok <- norm_token(dt_input)
    direct <- match(tok, norm_token(catg$values))
    if (!is.na(direct)) return(catg$values[direct])
    for (v in names(catg$syns)) if (tok %in% norm_token(catg$syns[[v]])) return(v)
    stop("❌ For data_source=", ds_label,
         " the `data_type` must be one of: ",
         paste(catg$values, collapse=", "),
         " (case-insensitive; synonyms accepted).")
  }

  # --- build/validate metadata ---
  if (is.null(log_metadata)) {
    if (is.null(data_source)) stop("❌ Provide `data_source` (or a `log_metadata` list).")
    log_metadata <- list(
      data_year       = data_year,        # optional, for audit only
      data_source     = data_source,
      data_description= data_description,
      user_note       = user_note
    )
  }
  if (is.null(log_metadata$data_description) || is.na(log_metadata$data_description) || log_metadata$data_description == "")
    stop("❌ Please provide `data_description`.")

  ds_label <- (function(x){
    tok <- norm_token(x)
    out <- c(assessment="Assessment", cde="CDE", dashboard="Dashboard")[tok]
    if (is.na(out)) stop("❌ `data_source` must be one of: Assessment, CDE, Dashboard.")
    out
  })(log_metadata$data_source)

  if (is.null(table_name)) stop("❌ Please supply `table_name`.")
  if (is.null(user_note) || !grepl("\\b(fact|dim)\\b", user_note, ignore.case = TRUE))
    stop("❌ `user_note` must include 'fact' or 'dim'.")
  table_type <- tolower(stringr::str_extract(user_note, "\\b(fact|dim)\\b"))
  if (is.na(table_type)) stop("❌ Could not parse table type from `user_note`.")
  if (!is.null(dim_description)) dim_description <- janitor::make_clean_names(dim_description)

  final_table_name <- paste0(
    table_name,
    if (!is.null(dim_description)) paste0("_", dim_description),
    "_", table_type
  )

  # --- auto path: T:/Data Warehouse/{DataSource}/{DataType}/ ---
  if (is.null(path)) {
    if (is.null(data_type)) stop("❌ Please provide `data_type` when `path` is NULL.")
    type_label  <- resolve_type(ds_label, data_type)
    type_folder <- title_underscore(type_label)
    base_dir    <- "T:/Data Warehouse"
    path <- file.path(base_dir, ds_label, type_folder, paste0(final_table_name, ".csv"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  if (compress && !grepl("\\.gz$", path)) path <- paste0(path, ".gz")

  # --- enforce character on code columns ---
  default_char_cols <- if (table_type == "dim")
    c("cds","county_code","district_code","school_code") else c("cds")
  target_char_cols <- intersect(unique(c(char_cols, default_char_cols)), names(data))
  if (length(target_char_cols))
    data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(target_char_cols), as.character))

  # --- write + quick preview ---
  data.table::fwrite(data, path)
  message("✅ File written: ", path)
  if (n_check > 0)
    print(data.table::fread(path, nrows = n_check, colClasses = list(character = target_char_cols)))

  # --- logging ---
  fi <- file.info(path)
  if (is.na(fi$size)) stop("File does not exist or is unreadable: ", path)

  canonical_table_id <- canonical_table_id %||% table_name
  dimension_type <- dimension_type %||% NA
  valid_dimension_types <- c("universal","annualized","other")
  if (!is.na(dimension_type) && !tolower(dimension_type) %in% valid_dimension_types)
    stop("❌ `dimension_type` must be one of: ", paste(valid_dimension_types, collapse = ", "))
  dimension_type <- if (is.na(dimension_type)) NA_character_ else tolower(dimension_type)

  log_entry <- data.frame(
    timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file_name        = basename(path),
    file_path        = normalizePath(path),
    file_size_MB     = round(fi$size / 1e6, 2),
    canonical_table_id = canonical_table_id,
    dimension_type   = dimension_type,
    n_rows           = nrow(data),
    n_cols           = ncol(data),
    data_year        = log_metadata$data_year %||% NA,
    data_source      = ds_label,
    table_type       = table_type,
    data_description = log_metadata$data_description,
    dim_description  = dim_description %||% NA,
    user_note        = user_note,
    user             = Sys.info()[["user"]],
    stringsAsFactors = FALSE
  )

  if (file.exists(log_path)) {
    existing_log <- read.csv(log_path, stringsAsFactors = FALSE)
    match_idx <- which(existing_log$canonical_table_id == canonical_table_id)
    if (length(match_idx)) {
      existing_log[match_idx[1], ] <- log_entry
      write.csv(existing_log, log_path, row.names = FALSE)
      message("🔁 Existing log entry overwritten for: ", log_entry$file_name)
    } else {
      write.table(log_entry, log_path, append = TRUE, sep = ",",
                  row.names = FALSE, col.names = FALSE)
      message("📝 Log entry appended to: ", log_entry$file_name)
    }
  } else {
    write.table(log_entry, log_path, append = FALSE, sep = ",",
                row.names = FALSE, col.names = TRUE)
    message("📄 New log created: ", log_path)
  }

  invisible(NULL)
}
