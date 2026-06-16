#' Resolve CDS conflicts where the same CDS code appears at the same org level with different school names
#'
#' Detects cases where multiple schools share the same CDS code AND the same organization level —
#' a conflict that `resolve_conflicting_cds()` does not catch because that function only flags
#' codes that span *different* org levels. A real-world example is three virtual schools in
#' San Joaquin County that all carry the same CDS code at org level `"S"`.
#'
#' Resolution strategy: within each conflicting (cds, org_level) group, schools are ranked
#' alphabetically by name. The first school (rank 1) retains the original `altered_cds` value.
#' Subsequent schools receive a numeric suffix (`"2"`, `"3"`, etc.) appended to `altered_cds`,
#' making each school's identifier unique. Alphabetical ranking ensures the mapping is
#' deterministic across repeated runs on the same data.
#'
#' @param df A data frame that has already been processed by `pad_cds_codes()` and
#'   `resolve_conflicting_cds()`, so that the `altered_cds` column already exists.
#' @param cds_col A string specifying the column name containing the CDS code. Default is `"cds"`.
#' @param org_level_col A string specifying the column name containing the organization level. Default is `"org_level"`.
#' @param school_col A string specifying the column name containing the school name. Default is `"school_name"`.
#' @param altered_col A string specifying the column name to update with suffixed values. Default is `"altered_cds"`.
#'
#' @return A modified data frame with `altered_cds` updated for any same-level conflicts.
#'   A `"same_level_conflicts"` attribute is attached to the returned data frame, containing
#'   a summary of detected conflicts (one row per conflicting CDS + org_level group).
#'   If no conflicts are detected the data frame is returned unchanged with an empty
#'   `"same_level_conflicts"` attribute.
#'
#' @export

resolve_same_level_cds <- function(df,
                                   cds_col       = "cds",
                                   org_level_col = "org_level",
                                   school_col    = "school_name",
                                   altered_col   = "altered_cds") {
  
  required_cols <- c(cds_col, org_level_col, school_col, altered_col)
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "resolve_same_level_cds() requires columns: ",
      paste(missing_cols, collapse = ", "),
      "\nEnsure pad_cds_codes() and resolve_conflicting_cds() have been run first."
    )
  }
  
  # Identify (cds, org_level) groups with more than one distinct school name
  conflicts <- df %>%
    dplyr::filter(!is.na(.data[[school_col]])) %>%
    dplyr::group_by(.data[[cds_col]], .data[[org_level_col]]) %>%
    dplyr::summarise(
      n_schools = dplyr::n_distinct(.data[[school_col]]),
      schools   = paste(sort(unique(.data[[school_col]])), collapse = " | "),
      .groups   = "drop"
    ) %>%
    dplyr::filter(n_schools > 1)
  
  if (nrow(conflicts) == 0) {
    message("resolve_same_level_cds: no same-level CDS conflicts detected.")
    attr(df, "same_level_conflicts") <- conflicts
    return(df)
  }
  
  message(
    "resolve_same_level_cds: ", nrow(conflicts),
    " same-level CDS conflict(s) detected. Appending suffixes to ", altered_col, "."
  )
  print(conflicts)
  
  # Build a suffix lookup: cds + org_level + school_name --> suffix string
  conflict_keys <- conflicts %>%
    dplyr::select(dplyr::all_of(c(cds_col, org_level_col)))
  
  suffix_map <- df %>%
    dplyr::inner_join(conflict_keys, by = c(cds_col, org_level_col)) %>%
    dplyr::filter(!is.na(.data[[school_col]])) %>%
    dplyr::distinct(
      .data[[cds_col]], .data[[org_level_col]], .data[[school_col]]
    ) %>%
    dplyr::group_by(.data[[cds_col]], .data[[org_level_col]]) %>%
    dplyr::arrange(.data[[school_col]], .by_group = TRUE) %>%
    dplyr::mutate(school_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cds_suffix = dplyr::if_else(school_rank == 1L, "", as.character(school_rank))    ) %>%
    dplyr::select(
      dplyr::all_of(c(cds_col, org_level_col, school_col)),
      cds_suffix
    )
  
  # Join suffix map and update altered_cds
  df <- df %>%
    dplyr::left_join(suffix_map, by = c(cds_col, org_level_col, school_col)) %>%
    dplyr::mutate(
      !!altered_col := dplyr::if_else(
        !is.na(cds_suffix) & cds_suffix != "",
        paste0(.data[[altered_col]], cds_suffix),
        as.character(.data[[altered_col]])
      )
    ) %>%
    dplyr::select(-cds_suffix)
  
  attr(df, "same_level_conflicts") <- conflicts
  df
}
