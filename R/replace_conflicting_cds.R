#' Resolve conflicting CDS codes by appending a suffix to school-level entries
#'
#' Detects CDS codes that appear at multiple organization levels and appends `"9999"` to the
#' `cds` code for school-level (`"S"`) records, allowing the dataset to preserve uniqueness.
#' This is typically used to resolve ambiguity in CDE datasets before loading into a warehouse.
#'
#' @param df A data frame containing the CDS column and org level column to modify.
#' @param conflicting_cds_df A data frame containing the list of conflicting CDS codes, typically returned by `get_conflicting_cds()`.
#' @param cds_col The name of the column that contains the CDS code. Default is `"cds"`.
#' @param org_level_col The name of the column that contains the org level. Default is `"org_level"`.
#'
#' @return A data frame with updated CDS values. Adds a temporary `altered_cds` column that marks modified rows with `1`.
#'
#' @export

replace_conflicting_cds <- function(df, conflicting_cds_df, cds_col = "cds", org_level_col = "org_level") {
  conflicting_cds_vec <- conflicting_cds_df %>% pull(!!sym(cds_col))

  df %>%
    mutate(
      altered_cds = if_else(
        .data[[cds_col]] %in% conflicting_cds_vec & .data[[org_level_col]] == "S",
        1L,
        0L
      ),
      !!cds_col := if_else(
        altered_cds == 1L,
        paste0(.data[[cds_col]], "9999"),
        as.character(.data[[cds_col]])
      )
    )
}
