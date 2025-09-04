#' Remove dimension-related objects from the global environment
#'
#' Scans the global environment for objects with names that begin with a user-defined `base_string`
#' and contain keywords like "dim", "entit", "group", or "label". It optionally removes them or
#' just displays them in a dry run. Useful for cleaning up intermediate or output objects created
#' during ETL or report generation.
#'
#' @param base_string A string that all targeted object names should begin with (e.g., `"ltel19"`, `"sbac24"`).
#' @param dry_run Logical. If `TRUE`, shows which objects would be removed without actually deleting them. Default is `TRUE`.
#' @param keywords A character vector of substrings to match within object names. Defaults to `c("dim", "entit", "group", "label")`.
#'
#' @return (Invisibly) A character vector of object names that matched the filters. If `dry_run = FALSE`, these objects are removed.
#'
#' @details
#' - This function uses pattern matching to find "dimension-like" objects.
#' - It searches for objects in the global environment (`.GlobalEnv`).
#' - Useful for scripts that generate many intermediate tables for reporting or dashboarding.
#'
#' @export

clear_dim_objects <- function(base_string = NULL, dry_run = TRUE,
                              keywords = c("dim", "entit", "group", "label")) { # these are same keywords it will check against
  if (is.null(base_string)) {
    cli::cli_alert_danger("You must supply a `base_string` to filter (e.g., 'ltel19', 'sbac24').")
    return(invisible(NULL))
  }

  all_objs <- ls(envir = .GlobalEnv)
  pattern <- paste(keywords, collapse = "|")
  dim_objs <- stringr::str_subset(all_objs, pattern)
  matching_objs <- stringr::str_subset(dim_objs, paste0("^", base_string))

  if (length(matching_objs) == 0) {
    cli::cli_alert_info("No matching dimension-like objects found for {.val {base_string}}.")
    return(invisible(NULL))
  }

  cli::cli_h1("Objects to be removed:")
  cli::cli_ul(matching_objs)

  if (!dry_run) {
    rm(list = matching_objs, envir = .GlobalEnv)
    cli::cli_alert_success("Removed {.val {length(matching_objs)}} objects.")
  } else {
    cli::cli_alert_info("This was a dry run. Set {.code dry_run = FALSE} to remove.")
  }

  invisible(matching_objs)
}
