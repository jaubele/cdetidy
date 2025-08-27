#' Generate deterministic foreign key constraint names from schema metadata
#'
#' Constructs constraint names for foreign keys based on a flagged schema data frame, handling
#' both single-column and multi-column (composite) foreign key relationships. Returns a character
#' vector of cleaned and deterministic names suitable for use in SQL table creation scripts.
#'
#' @param schema_flagged A data frame containing schema metadata, with columns:
#' - `table_name`: name of the table
#' - `column_name`: name of the column
#' - `is_primary_key`: "YES" or "NO" for PK status
#' - `is_foreign_key`: "YES" or "NO" for FK status
#' - `foreign_key_ref`: reference target(s), formatted like `"table(col)"` or `"table1(col1) | table2(col2)"`
#'
#' @return A character vector of foreign key constraint names. Each name follows the format:
#' - For single-column FKs: `fk_<child_table>_<child_column>_<ref_table>`
#' - For multi-column composite FKs: `fk_<child_table>_<ref_table>_<ordered_child_cols>`
#'
#' @details
#' - Constraint names are made SQL-safe by replacing invalid characters with underscores.
#' - Composite constraints are only generated if the referenced columns match the full PK of the referenced table.
#' - Duplicate or malformed references will result in an error.
#'
#' @examples
#' \dontrun{
#' fk_constraint_names(schema_flagged_df)
#' }
#'
#' @export

fk_constraint_names <- function(schema_flagged) {
  # parent PK map
  pk_cols <- schema_flagged %>%
    dplyr::filter(.data$is_primary_key == "YES") %>%
    dplyr::group_by(.data$table_name) %>%
    dplyr::mutate(pk_order = dplyr::row_number()) %>%   # order of appearance within each table
    dplyr::arrange(.data$table_name, .data$pk_order) %>%
    dplyr::summarise(ref_pk_cols = list(.data$column_name), .groups = "drop")

  # parse FK rows
  fk_rows <- schema_flagged %>%
    dplyr::filter(.data$is_foreign_key == "YES", !is.na(.data$foreign_key_ref))

  if (nrow(fk_rows) == 0) return(character(0))

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
  ) %>% dplyr::distinct() %>%
    dplyr::left_join(pk_cols, by = c("ref_table" = "table_name")) %>%
    dplyr::arrange(.data$table_name, .data$ref_table, .data$column_name, .data$ref_column)

  safe_name <- function(x) {gsub("[^A-Za-z0-9_]", "_", x)}

  # names for singles
  single_name <- function(child_tbl, child_col, ref_tbl) {
    safe_name(paste0("fk_", child_tbl, "_", child_col, "_", ref_tbl))
  }

  # names for composites
  composite_name <- function(child_tbl, ref_tbl, child_cols_ordered) {
    safe_name(paste0("fk_", child_tbl, "_", ref_tbl, "_",
                     paste(child_cols_ordered, collapse = "_")))
  }

  # deterministic grouping key (same as coder)
  key <- interaction(parsed$table_name, parsed$ref_table, drop = TRUE, lex.order = TRUE)
  groups <- split(parsed, key, drop = TRUE)

  names_out <- unlist(purrr::map(groups, function(df) {
    child_tbl <- df$table_name[1]
    ref_tbl   <- df$ref_table[1]
    child_cols <- df$column_name
    ref_cols   <- df$ref_column

    has_pk <- !vapply(df$ref_pk_cols, function(x) is.null(x) || all(is.na(x)), logical(1))
    pk <- if (any(has_pk)) df$ref_pk_cols[[ which(has_pk)[1] ]] else NULL

    if (!is.null(pk) && length(child_cols) >= 2 && setequal(ref_cols, pk)) {
      ord <- match(pk, ref_cols)
      child_ordered <- child_cols[ord]
      composite_name(child_tbl, ref_tbl, child_ordered)
    } else {
      purrr::map2_chr(child_cols, ref_cols, ~ single_name(child_tbl, .x, ref_tbl))
    }
  }))

  unique(unname(names_out))
}
