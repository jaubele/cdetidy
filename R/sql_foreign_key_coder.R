#' Generate SQL scripts to add foreign key constraints
#'
#' Based on a schema metadata data frame (e.g., from `sql_schema_log()`), this function generates
#' T-SQL `ALTER TABLE` statements to define foreign key relationships. It supports both
#' single-column and composite key relationships and avoids duplicate constraint creation using
#' `IF NOT EXISTS` checks. The output is suitable for use in post-load SQL warehouse scripts.
#'
#' @param schema_flagged A data frame containing schema metadata, including the following columns:
#' - `table_name`: Name of the child table
#' - `column_name`: Foreign key column in the child table
#' - `is_foreign_key`: Logical or "YES"/"NO" string
#' - `foreign_key_ref`: Reference in `"table(column)"` or `"table1(col1) | table2(col2)"` format
#' - `is_primary_key`: Indicates primary key status for composite detection
#'
#' @return A character vector of SQL `ALTER TABLE` statements to define foreign key constraints.
#' Each statement begins with an `IF NOT EXISTS` check to avoid duplicate creation.
#'
#' @details
#' - Composite keys are supported if the referenced columns match the full PK of the referenced table.
#' - Names are cleaned for SQL compatibility using safe alphanumeric formatting.
#' - The function uses `glue::glue()` for string assembly and assumes the presence of `sql_ident()` for quoting identifiers.
#'
#' @export

sql_foreign_key_coder <- function(schema_flagged) {

  # 1) Pull PK columns for each table (ordered by appearance)
  pk_cols <- schema_flagged %>%
    dplyr::filter(.data$is_primary_key == "YES") %>%
    dplyr::group_by(.data$table_name) %>%
    dplyr::mutate(pk_order = dplyr::row_number()) %>%   # order of appearance within each table
    dplyr::arrange(.data$table_name, .data$pk_order) %>%
    dplyr::summarise(ref_pk_cols = list(.data$column_name), .groups = "drop")

  # 2) Rows flagged as FKs
  fk_rows <- schema_flagged %>%
    dplyr::filter(.data$is_foreign_key == "YES", !is.na(.data$foreign_key_ref))

  if (nrow(fk_rows) == 0) {
    cat("ℹ️ No foreign key references found in schema.\n")
    return(character(0))
  }

  # 3) Parse "tbl(col) | other_tbl(col)" cells into rows
  parsed <- purrr::pmap_dfr(
    fk_rows,
    function(table_name, column_name, foreign_key_ref, ...) {

      refs <- strsplit(foreign_key_ref, "\\s*\\|\\s*")[[1]]

      purrr::map_dfr(seq_along(refs), function(i) {

        ref <- trimws(refs[[i]])

        if (!grepl("^[^()]+\\([^()]+\\)$", ref)) {
          stop("❌ Malformed foreign_key_ref for [", table_name, ".", column_name, "]: ", ref)
        }

        ref_table  <- trimws(sub("\\(.*$", "", ref))
        ref_column <- trimws(sub("^.*\\(", "", sub("\\)$", "", ref)))

        tibble::tibble(table_name, column_name, ref_table, ref_column)
      })
    }
  ) %>%
    dplyr::distinct()

  if (nrow(parsed) == 0) {
    cat("ℹ️ Parsed 0 foreign key references.\n")
    return(character(0))
  }

  # 4) Attach parent PKs to each ref_table
  parsed <- parsed %>%
    dplyr::left_join(pk_cols, by = c("ref_table" = "table_name")) %>%
    dplyr::arrange(.data$table_name, .data$ref_table, .data$column_name, .data$ref_column)

  # helpers
  safe_name <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

  cname_single    <- function(child_tbl, child_col, ref_tbl)
    safe_name(paste0("fk_", child_tbl, "_", child_col, "_", ref_tbl))

  cname_composite <- function(child_tbl, ref_tbl, child_cols_ordered)
    safe_name(paste0("fk_", child_tbl, "_", ref_tbl, "_", paste(child_cols_ordered, collapse = "_")))

  emit_single <- function(child_tbl, ref_tbl, c_col, r_col) {
    nm <- cname_single(child_tbl, c_col, ref_tbl)
    glue::glue(
      "IF NOT EXISTS (SELECT 1 FROM sys.foreign_keys WHERE name = '{nm}')",
      "BEGIN",
      "  ALTER TABLE {sql_ident(child_tbl)}",
      "  ADD CONSTRAINT {sql_ident(nm)}",
      "    FOREIGN KEY ({sql_ident(c_col)})",
      "    REFERENCES {sql_ident(ref_tbl)}({sql_ident(r_col)});",
      "END",
      .sep = "\n"
    )
  }

  emit_composite <- function(child_tbl, ref_tbl, child_ordered, ref_ordered) {
    nm <- cname_composite(child_tbl, ref_tbl, child_ordered)
    fk_cols  <- paste(sql_ident(child_ordered), collapse = ", ")
    ref_cols <- paste(sql_ident(ref_ordered),   collapse = ", ")
    glue::glue(
      "IF NOT EXISTS (SELECT 1 FROM sys.foreign_keys WHERE name = '{nm}')",
      "BEGIN",
      "  ALTER TABLE {sql_ident(child_tbl)}",
      "  ADD CONSTRAINT {sql_ident(nm)}",
      "    FOREIGN KEY ({fk_cols})",
      "    REFERENCES {sql_ident(ref_tbl)}({ref_cols});",
      "END",
      .sep = "\n"
    )
  }

  # 5) Build SQL per (child table, ref table)
  # explicit deterministic grouping key
  key <- interaction(parsed$table_name,
                     parsed$ref_table,
                     drop = TRUE,
                     lex.order = TRUE)

  groups <- split(parsed, key, drop = TRUE)

  out_sql <- purrr::map_chr(groups, function(df) {
    child_tbl <- df$table_name[1]
    ref_tbl   <- df$ref_table[1]

    # pairs (child -> ref)
    child_cols <- df$column_name
    ref_cols   <- df$ref_column

    # safer NA/NULL handling for list-column from join
    has_pk <- !vapply(df$ref_pk_cols, function(x) is.null(x) || all(is.na(x)), logical(1))
    pk <- if (any(has_pk)) df$ref_pk_cols[[ which(has_pk)[1] ]] else NULL

    if (!is.null(pk) && length(child_cols) >= 2 && setequal(ref_cols, pk)) {
      ord <- match(pk, ref_cols)   # align to parent PK order
      child_ordered <- child_cols[ord]
      ref_ordered   <- ref_cols[ord]
      emit_composite(child_tbl, ref_tbl, child_ordered, ref_ordered)
    } else {
      paste(purrr::map2_chr(child_cols, ref_cols,
                            ~ emit_single(child_tbl, ref_tbl, .x, .y)),
            collapse = "\n\n")
    }
  })

  unname(unlist(out_sql))

}
