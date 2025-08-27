#' Identify and resolve conflicting CDS codes in a dataset
#'
#' Wraps `get_conflicting_cds()` and `replace_conflicting_cds()` to identify duplicate CDS codes
#' that span multiple organization levels and resolve them by appending a suffix (e.g., `"9999"`)
#' to the school-level (`"S"`) CDS codes. The resulting dataset preserves uniqueness while maintaining traceability.
#'
#' @param df A data frame containing CDS codes and organization level indicators.
#' @param cds_col A string specifying the column name containing the CDS code.
#' @param org_level_col A string specifying the column name containing the organization level (e.g., `"C"`, `"D"`, `"S"`).
#'
#' @return A modified data frame with resolved CDS values. A `"conflicting_cds"` attribute is attached
#' to the returned data frame, containing the list of detected conflicts.
#'
#' @export

resolve_conflicting_cds <- function(df, cds_col, org_level_col) {
  # Step 1: Identify conflicting CDS codes
  conflicting_cds_df <- get_conflicting_cds(df, cds_col = cds_col, org_level_col = org_level_col)

  # Step 2: Modify CDS values in the dataset
  df_modified <- replace_conflicting_cds(df, conflicting_cds_df, cds_col = cds_col, org_level_col = org_level_col)

  # Optional: store conflicts as an attribute for later access
  attr(df_modified, "conflicting_cds") <- conflicting_cds_df

  return(df_modified)
}
