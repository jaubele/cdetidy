#' Identify and resolve conflicting CDS codes in a dataset
#'
#' Wraps `get_conflicting_cds()`, `replace_conflicting_cds()`, and `resolve_same_level_cds()`
#' to detect and resolve two distinct types of CDS conflict:
#'
#' \describe{
#'   \item{Cross-level conflicts}{The same CDS code appears at more than one organization level
#'     (e.g., both `"D"` and `"S"`). Resolved by `get_conflicting_cds()` /
#'     `replace_conflicting_cds()`, which appends `"9999"` to the school-level (`"S"`) CDS.}
#'   \item{Same-level conflicts}{The same CDS code appears at the same organization level but
#'     with different school names (e.g., multiple virtual schools sharing one CDS at level `"S"`).
#'     Resolved by `resolve_same_level_cds()`, which appends `"2"`, `"3"`, etc. to
#'     `altered_cds` for the second and subsequent schools (ranked alphabetically).}
#' }
#'
#' @param df A data frame containing CDS codes and organization level indicators.
#' @param cds_col A string specifying the column name containing the CDS code.
#' @param org_level_col A string specifying the column name containing the organization level
#'   (e.g., `"C"`, `"D"`, `"S"`).
#' @param school_col A string specifying the column name containing the school name, used by
#'   `resolve_same_level_cds()`. Default is `"school_name"`.
#' @param altered_col A string specifying the column name that holds the modified CDS values.
#'   Default is `"altered_cds"`.
#'
#' @return A modified data frame with resolved CDS values. Two attributes are attached:
#' \describe{
#'   \item{`"conflicting_cds"`}{Summary of cross-level conflicts from `get_conflicting_cds()`.}
#'   \item{`"same_level_conflicts"`}{Summary of same-level conflicts from `resolve_same_level_cds()`.}
#' }
#'
#' @export

resolve_conflicting_cds <- function(df,
                                    cds_col       = "cds",
                                    org_level_col = "org_level",
                                    school_col    = "school_name",
                                    altered_col   = "altered_cds") {
  
  # Step 1: Identify cross-level conflicting CDS codes
  conflicting_cds_df <- get_conflicting_cds(df, cds_col = cds_col, org_level_col = org_level_col)
  
  # Step 2: Resolve cross-level conflicts (appends "9999" to school-level rows)
  df_modified <- replace_conflicting_cds(df, conflicting_cds_df, cds_col = cds_col, org_level_col = org_level_col)
  
  # Store cross-level conflicts as an attribute for later access
  attr(df_modified, "conflicting_cds") <- conflicting_cds_df
  
  # Step 3: Resolve same-level conflicts (same CDS + org_level, different school names)
  df_modified <- resolve_same_level_cds(df_modified,
                                        cds_col       = cds_col,
                                        org_level_col = org_level_col,
                                        school_col    = school_col,
                                        altered_col   = altered_col)
  
  return(df_modified)
}