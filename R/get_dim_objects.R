#' List dimension-like objects in the global environment
#'
#' Searches the global environment for object names that contain dimension-related keywords
#' such as `"dim"`, `"entit"`, `"group"`, or `"label"`, and returns them (excluding known function names).
#'
#' @param keywords A character vector of keywords to search for within object names. Default includes `"dim"`, `"entit"`, `"group"`, and `"label"`.
#'
#' @return Invisibly returns a character vector of matching object names. Also prints a styled summary using `cli`.
#'
#' @details
#' - Objects are matched using a keyword-based regular expression.
#' - Known function names are excluded from the results.
#' - This function is useful for interactively auditing which dimension-like data frames exist in memory.
#'
#' @export

get_dim_objects <- function(keywords = c("dim", "entit", "group", "label")) { # these are keywords it will check against
  all_objs <- ls(envir = .GlobalEnv)
  pattern <- paste(keywords, collapse = "|")

  dim_objs <- stringr::str_subset(all_objs, pattern)
  dim_objs <- setdiff(dim_objs, c("get_dim_objects", "clear_dim_objects",
                                  "assessment_files_group_labeling",
                                  "cde_files_group_labeling",
                                  "dashboard_files_group_labeling",
                                  "validate_group_mapping_tabyl"))  # Exclude known functions

  if (length(dim_objs) > 0) {
    cli::cli_h1("🗂️  Dimension-like Objects Found")
    cli::cli_ul(dim_objs)
  } else {
    cli::cli_alert_info("No matching dimension-like objects found.")
  }

  return(invisible(dim_objs))
}
