#' Filter a schema log for a specific data source (with optional universal tables)
#'
#' Filters a schema metadata table (typically from `primary_sql_schema_log.csv`) to include
#' only rows that match a specified `data_source`, optionally including "universal" dimensions.
#'
#' @param schema_df A data frame representing the full schema log, including columns `data_source`, `dimension_type`, and `table_name`.
#' @param data_source A string or character vector specifying one or more of `"assessment"`, `"cde"`, or `"dashboard"`. Required.
#' @param include_universal Logical. If `TRUE`, also includes rows where `dimension_type == "universal"`. Default is `TRUE`.
#'
#' @return A filtered data frame containing only schema rows matching the specified data source(s),
#' plus universal tables if `include_universal = TRUE`.
#'
#' @details
#' - The function normalizes `data_source` labels to match the schema log.
#' - It will error if no matching rows are found or if an invalid `data_source` is provided.
#' - Useful for generating SQL, inserts, or table builds scoped to a single source system.
#'
#' @examples
#' \dontrun{
#' schema_log <- readr::read_csv("primary_sql_schema_log.csv")
#' filtered <- filter_schema_sources(schema_log, data_source = "assessment")
#' }
#'
#' @export

filter_schema_sources <- function(schema_df, data_source = NULL, include_universal = TRUE) {
  # Accept NULL (means "all"), single value, or vector: c("assessment","cde", "dashboard")
  if (is.null(data_source)) {
    stop("❌ You must explicitly specify data_source = assessment, cde, or dashboard.")
  }

  # Map of allowed keys -> labels used in the schema log
  key_to_label <- c(assessment = "Assessment", cde = "CDE", dashboard = "Dashboard")

  req <- tolower(as.character(data_source))
  bad <- setdiff(req, names(key_to_label))
  if (length(bad) > 0) {
    stop("❌ `data_source` must be : assessment, cde, dashboard. Invalid: ",
         paste(bad, collapse = ", "))
  }

  # Normalize schema_df$data_source to the canonical labels just in case
  schema_norm <- schema_df %>%
    dplyr::mutate(
      data_source = dplyr::case_when(
        tolower(data_source) %in% names(key_to_label) ~ key_to_label[tolower(data_source)],
        TRUE ~ as.character(data_source)
      ),
      dimension_type = tolower(trimws(as.character(dimension_type)))
    )

  wanted_labels <- unname(key_to_label[req])

  filtered <- schema_norm %>%
    dplyr::filter(data_source %in% wanted_labels |
                    (include_universal & dimension_type == "universal")
    )

  if (nrow(filtered) == 0) {
    stop("⚠️ No rows in primary_sql_schema_log.csv match data_source = ",
         paste(wanted_labels, collapse = ", "), " (plus universal).")
  }

  cat("📦 Data sources selected: ", paste(wanted_labels, collapse = ", "),
      if(include_universal) " + universal" else "",
      " — ", length(unique(filtered$table_name)), " table(s).\n", sep = "")

  filtered
}
