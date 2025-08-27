#' Load, compare, and optionally join multiple files from a folder
#'
#' Searches a folder for files matching given keywords, loads them into memory,
#' checks for consistency in column names and order, and optionally joins them
#' using one of several join methods. Designed for validating files before merging
#' in an ETL or QA process.
#'
#' @param folder Path to the folder where files will be searched.
#' @param keywords A character vector of keywords to match filenames against.
#' @param join_method Join strategy to use. One of `"bind_rows"` (default), `"left_join"`, or `"full_join"`.
#' @param by If using a join method (not `bind_rows`), a character vector of join keys. Required for joins.
#' @param recursive Logical. If `TRUE`, search subdirectories. Default is `TRUE`.
#' @param ignore_case Logical. If `TRUE`, ignore case when matching filenames. Default is `TRUE`.
#' @param full_names Logical. If `TRUE`, return full file paths. Default is `TRUE`.
#' @param require_approval_on_diff Logical. If `TRUE` and differences in column names/order are found, prompts for user confirmation. Default is `TRUE`.
#' @param force Logical. If `TRUE`, override prompts and proceed even when discrepancies are found. Default is `FALSE`.
#' @param encoding Optional. Encoding to use when reading files (e.g., `"UTF-8"`). Default is `"UTF-8"`.
#' @param id_col Optional. If provided, a column will be added when using `bind_rows` to indicate source file names.
#' @param show_compare_details Logical. If `TRUE`, prints detailed column comparison results. Default is `TRUE`.
#' @param also_run_compare_function Logical. If `TRUE`, and the function `compare_variable_names()` exists, it will be called for each file pair.
#'
#' @return A data frame created by joining the selected files (if no discrepancies) or `NULL` if discrepancies are found and approval is denied.
#' If `bind_rows` is used with `id_col`, the output will include a source ID column.
#'
#' @details
#' - Forces select columns (e.g., `"cds"`, `"county_code"`, etc.) to character type on load, if present.
#' - Normalizes character encodings to UTF-8.
#' - Prompts user to resolve column mismatches unless `force = TRUE` or run non-interactively.
#'
#' @export

load_and_compare_files <- function(
    folder,
    keywords,
    join_method = c("bind_rows", "left_join", "full_join"),
    by = NULL,
    recursive = TRUE,
    ignore_case = TRUE,
    full_names = TRUE,
    require_approval_on_diff = TRUE,
    force = FALSE,
    encoding = NULL,
    id_col = "source_file",
    show_compare_details = TRUE,
    also_run_compare_function = FALSE) {

  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
  join_method <- match.arg(join_method)

  selected_files <- files_by_keywords(
    folder = folder,
    keywords = keywords,
    recursive = recursive,
    ignore_case = ignore_case,
    full_names = full_names
  )
  if (length(selected_files) == 0) {
    message("🔍 No files matched the keywords: ", paste(keywords, collapse = ", "))
    return(invisible(NULL))
  }
  if (join_method == "left_join" && length(selected_files) < 2) {
    stop("❌ Need at least two files for left_join.")
  }
  if (join_method == "full_join" && length(selected_files) < 2) {
    stop("❌ Need at least two files for full_join.")
  }

  # ---- read all dfs (force key code columns to character) ----
  dfs <- lapply(
    selected_files,
    function(p) {

      loc <- readr::locale(encoding = encoding %||% "UTF-8")
      force_char_cols <- c("cds","county_code","district_code","school_code")

      # Peek header to know which forced columns are present (case-insensitive)
      hdr <- readr::read_csv(p, n_max = 0, show_col_types = FALSE, locale = loc)

      hdr_names <- names(hdr)

      hdr_names_clean <- trimws(hrd_names)

      present_idx <- which(tolower(hdr_names_clean) %in% force_char_cols)

      present_names <- hdr_names[present_idx]

      if (length(present_names)) {
        # Build a col_types spec: guess everything, override these to character
        spec <- readr::cols(.default = readr::col_guess())
        for (nm in present_names) spec$cols[[nm]] <- readr::col_character()
        df <- readr::read_csv(p, show_col_types = FALSE, locale = loc, col_types = spec)
      } else {
        df <- readr::read_csv(p, show_col_types = FALSE, locale = loc)
      }

      # Normalize all character data to UTF-8 (as you were doing)
      if (ncol(df)) {
        df <- dplyr::mutate(df, dplyr::across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8")))
      }
      df
    }
  )

  names(dfs) <- basename(selected_files)

  ref_name <- names(dfs)[1]
  ref_df   <- dfs[[1]]
  has_discrepancies <- FALSE

  if (length(dfs) >= 2) {
    for (i in 2:length(dfs)) {
      compare_name <- names(dfs)[i]
      compare_df   <- dfs[[i]]

      cols_ref <- names(ref_df)
      cols_cmp <- names(compare_df)

      only_in_ref <- setdiff(cols_ref, cols_cmp)
      only_in_cmp <- setdiff(cols_cmp, cols_ref)

      same_names <- setequal(cols_ref, cols_cmp)
      same_order <- identical(cols_ref, cols_cmp)

      if (show_compare_details) {
        cat("\n=============================================================\n")
        cat("📋 Comparing ", ref_name, "  vs  ", compare_name, "\n", sep = "")
        cat("-------------------------------------------------------------\n")

        if (length(only_in_ref)) {
          cat("In ", ref_name, " but not in ", compare_name, ":\n  - ",
              paste(only_in_ref, collapse = "\n  - "), "\n", sep = "")
        } else {
          cat("No variables in ", ref_name, " that are missing from ", compare_name, "\n", sep = "")
        }

        if (length(only_in_cmp)) {
          cat("\nIn ", compare_name, " but not in ", ref_name, ":\n  - ",
              paste(only_in_cmp, collapse = "\n  - "), "\n", sep = "")
        } else {
          cat("\nNo variables in ", compare_name, " that are missing from ", ref_name, "\n", sep = "")
        }

        if (same_names && !same_order) {
          cat("\n⚠️ Column names are identical but appear in a different order.\n")
        }

        cat("=============================================================\n")
      }

      if (!same_names) {
        has_discrepancies <- TRUE
      } else if (!same_order) {
        has_discrepancies <- TRUE
        message("⚠️ Column names are the same, but order differs between ", ref_name, " and ", compare_name)
      }

      if (also_run_compare_function && exists("compare_variable_names", mode = "function")) {
        tmp <- new.env(parent = .GlobalEnv)
        ref_sym <- as.name(make.names(ref_name))
        cmp_sym <- as.name(make.names(compare_name))
        assign(as.character(ref_sym), ref_df,     envir = tmp)
        assign(as.character(cmp_sym), compare_df, envir = tmp)
        eval(call("compare_variable_names", ref_sym, cmp_sym), envir = tmp)
      }
    }
  }

  if (has_discrepancies && require_approval_on_diff && !force) {
    if (!interactive()) {
      message("\n⚠️ Differences detected. Aborting join in non-interactive session.")
      return(invisible(NULL))
    }
    resp <- readline("❗ Differences detected (name or order). Proceed with join anyway? (yes/no): ")
    if (tolower(trimws(resp)) != "yes") {
      message("\n⚠️ Please resolve column differences before joining.")
      return(invisible(NULL))
    }
  }

  if (join_method == "bind_rows") {
    if (is.null(id_col)) {
      return(dplyr::bind_rows(dfs))
    } else {
      return(dplyr::bind_rows(dfs, .id = id_col))
    }
  } else {
    if (is.null(by)) stop("❌ For left_join, you must supply `by` with join key(s).")
    out <- dfs[[1]]
    for (i in 2:length(dfs)) {
      out <- switch(
        join_method,
        left_join = dplyr::left_join(out, dfs[[i]], by = by),
        full_join = dplyr::full_join(out, dfs[[i]], by = by),
        stop("❌ Unsupported join_method: ", join_method)
      )
    }

    return(out)
  }
}
