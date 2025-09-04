#' Validate a joined vs. distinct dataset and return the deduplicated frame
#'
#' Creates a fully distinct version of a *joined* dataset (across all columns),
#' verifies that every ID (optionally by year) present in the joined data
#' also appears in the distinct data, and (optionally) checks that the
#' supplied key columns form a primary key on the distinct data.
#'
#' This is meant for annualized “combined” dims where multiple files/years are
#' row-bound first, then deduplicated into a single canonical table.
#' Informative messages are printed with **cli**.
#'
#' @param data A data frame that represents the **joined** data (may contain duplicates).
#' @param key_cols Character vector of columns that should uniquely identify rows
#'   in the *distinct* data (e.g., `c("cds","year")`, `c("district_code","year")`).
#' @param check_pk Logical; if `TRUE` (default), run
#'   [validate_primary_key()] on the distinct data and report the result.
#'   Extra arguments are forwarded via `...`.
#' @param include_year Logical; when `TRUE` (default) and a column named
#'   `"year"` exists in both data frames, coverage checks use `(id, year)`
#'   pairs; otherwise they use the ID alone.
#' @param id_cols Character vector of ID columns to audit for coverage.
#'   Defaults to `c("cds","district_code","school_code")`. Only IDs that are
#'   actually present in `data` are checked.
#' @param show_examples Integer; how many example missing keys to print if any
#'   joined keys are not found in the distinct data. Default `10`.
#' @param ... Additional arguments passed to [validate_primary_key()]
#'   (e.g., `sample_n`, `full_run`, `seed`).
#'
#' @return The **distinct** data frame (`dplyr::distinct(data)`), invisibly
#'   printing coverage and (optionally) primary-key validation messages.
#'
#' @details
#' - Coverage check: for each ID in `id_cols` that exists in `data`, confirms
#'   that all `(id[, year])` keys from the joined data also appear in the
#'   distinct data; prints examples if not.
#' - Primary-key check: if `check_pk = TRUE`, calls
#'   [validate_primary_key(distinct_df, key_cols, ...)] and reports success/failure.
#'
#' @seealso [validate_primary_key()]
#'
#' @examples
#' \dontrun{
#' # Districts example
#' districts_distinct <- validate_combined_data(
#'   data = assessment_districts_joined,
#'   key_cols = c("district_code", "year"),
#'   full_run = TRUE
#' )
#'
#' # Schools example (sample PK check for speed)
#' schools_distinct <- validate_combined_data(
#'   data = assessment_schools_joined,
#'   key_cols = c("school_code", "year"),
#'   sample_n = 10000, full_run = FALSE
#' )
#' }
#'
#' @importFrom dplyr distinct anti_join
#' @importFrom cli cli_alert_success cli_alert_danger
#' @export

validate_combined_data <- function(
    data,
    key_cols,
    check_pk = TRUE,
    include_year = TRUE,
    id_cols = c("cds","district_code","school_code"),
    show_examples = 10,
    ...
) {
  # data: the *joined* data frame
  # key_cols: columns that should uniquely ID rows (e.g., c("cds","year") or c("district_code","year"))
  # check_pk: run validate_primary_key(data, key_cols, ...) and STOP if it fails
  # include_year: when TRUE and 'year' exists, coverage checks use (id, year); else just id
  # id_cols: which ID columns to audit if present
  # ... : extra args forwarded to validate_primary_key (e.g., sample_n, full_run, seed)

  stopifnot(is.data.frame(data))

  # 0) Column presence for the PK check
  if (!all(key_cols %in% names(data))) {
    missing_cols_str <- paste(setdiff(key_cols, names(data)), collapse = ", ")
    cli::cli_alert_danger("❌ Missing key columns in `data`: {missing_cols_str}.")
  }

  # 1) Create distinct df (across all columns)
  distinct_df <- dplyr::distinct(data)

  # 3) Coverage checks for whichever ID columns exist
  present_ids <- intersect(id_cols, names(data))
  if (!length(present_ids)) {
    id_cols_str <- paste(id_cols, collapse = ", ")
    cli::cli_alert_danger("❌ None of the expected ID columns found: {id_cols_str}.")
  }

  has_year <- include_year && ("year" %in% names(data)) && ("year" %in% names(distinct_df))

  check_one <- function(idc) {

    keys <- if (has_year) c(idc, "year") else idc
    keys_str <- paste(keys, collapse = ", ")

    j_keys <- unique(as.data.frame(data[, keys, drop = FALSE]))
    d_keys <- unique(as.data.frame(distinct_df[, keys, drop = FALSE]))

    # Ensure every key from JOINED appears in DISTINCT
    missing_in_distinct <- dplyr::anti_join(j_keys, d_keys, by = keys)

    if (nrow(missing_in_distinct) == 0) {
      n_ok <- nrow(j_keys)
      cli::cli_alert_success(
        "Successful join for [{keys_str}]: all {n_ok} key(s) from JOINED appear in DISTINCT."
      )

      list(id = idc, keys = keys, pass = TRUE,
           joined_n = nrow(j_keys), distinct_n = nrow(d_keys),
           missing = missing_in_distinct)

    } else {
      n_miss <- nrow(missing_in_distinct)
      cli::cli_alert_danger(
        "❌ {n_miss} key(s) from JOINED are missing in DISTINCT for [{keys_str}]. Examples:"
      )

      print(utils::head(missing_in_distinct, show_examples))

      list(id = idc, keys = keys, pass = FALSE,
           joined_n = nrow(j_keys), distinct_n = nrow(d_keys),
           missing = missing_in_distinct)
    }
  }

  # 3) Optional PK validation on DISTINCT

  if (isTRUE(check_pk)) {

    pk_res <- tryCatch(validate_primary_key(distinct_df, key_cols, ...), error = identity)

    if (inherits(pk_res, "error")) stop(pk_res$message)

    if (is.list(pk_res) && isFALSE(pk_res$pass)) {
      cols_str <- paste(key_cols, collapse = ", ")
      cli::cli_alert_danger(
        "Primary key check failed on DISTINCT data. Fix duplicates/NA in: {cols_str}."
      )
    }

    if (is.list(pk_res) && isTRUE(pk_res$pass)) {
      cols_str <- paste(key_cols, collapse = ", ")
      cli::cli_alert_success(
        "Primary key holds on DISTINCT for [{cols_str}]."
      )
    }
  }

  invisible(lapply(present_ids, check_one))

  distinct_df

}
