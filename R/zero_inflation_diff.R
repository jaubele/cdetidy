#' Compare zero-value rates across two datasets
#'
#' Calculates and compares the proportion of zero values in numeric (or numeric-like)
#' columns between two datasets (typically across years or versions). Flags variables
#' whose zero rates differ by a threshold, optionally within groups.
#'
#' @param df_prev A data frame representing the "previous" or baseline dataset.
#' @param df_curr A data frame representing the "current" or updated dataset.
#' @param group_var Optional. A character string specifying a grouping variable (e.g., `"district"` or `"cds"`).
#'   If `NULL` (default), zero-rate comparison is performed at the overall level.
#' @param delta_threshold A numeric value (default `0.05`) representing the minimum absolute change in
#'   zero-rate that will be flagged.
#' @param numeric_cols Optional. A character vector of column names to limit comparison to specific
#'   numeric columns. If `NULL`, the function auto-detects numeric or numeric-like columns.
#' @param verbose Logical. If `TRUE`, prints information about which columns are being compared.
#'
#' @return A data frame with one row per group-variable/metric combination, including:
#' \describe{
#'   \item{n_prev, zero_rate_prev}{Number of values and proportion of zero values in `df_prev`}
#'   \item{n_curr, zero_rate_curr}{Same for `df_curr`}
#'   \item{delta_zero_rate}{Difference in zero rates}
#'   \item{flag_large_delta}{1 if the delta exceeds `delta_threshold`, else 0}
#' }
#'
#' @examples
#' prev <- data.frame(a = c(0, 1, 2), b = c(0, 0, 1))
#' curr <- data.frame(a = c(0, 0, 0), b = c(1, 1, 1))
#' zero_inflation_diff(prev, curr, delta_threshold = 0.2)
#'
#' @export

zero_inflation_diff <- function(df_prev, df_curr,
                                group_var = NULL,
                                delta_threshold = 0.05,
                                numeric_cols = NULL,
                                verbose = FALSE,
                                from_enc = "latin1") {
  
  # --- helpers ---
  sanitize_df <- function(x, from = "latin1") {
    if (inherits(x, "data.table")) x <- as.data.frame(x)
    # fix column names
    names(x) <- enc2utf8(iconv(names(x), from = from, to = "UTF-8", sub = "byte"))
    # fix character columns
    chr_idx <- vapply(x, is.character, logical(1))
    if (any(chr_idx)) {
      x[chr_idx] <- lapply(x[chr_idx], function(v) enc2utf8(iconv(v, from = from, to = "UTF-8", sub = "byte")))
    }
    tibble::as_tibble(x)
  }
  is_numlike <- function(v) is.numeric(v) || inherits(v, "integer64") ||
    (is.character(v) && any(!is.na(suppressWarnings(as.numeric(v)))))
  
  # --- sanitize encodings up front ---
  df_prev <- sanitize_df(df_prev, from_enc)
  df_curr <- sanitize_df(df_curr, from_enc)
  
  # --- choose metrics ---
  shared <- intersect(names(df_prev), names(df_curr))
  if (!is.null(numeric_cols)) {
    target_cols <- intersect(shared, numeric_cols)
  } else {
    num_prev <- shared[vapply(df_prev[shared], is_numlike, logical(1))]
    num_curr <- shared[vapply(df_curr[shared], is_numlike, logical(1))]
    target_cols <- intersect(num_prev, num_curr)
  }
  if (length(target_cols) == 0) stop("No shared numeric(-like) columns found. Pass numeric_cols explicitly.")
  
  # --- compute zero rates for each side ---
  calc_rate <- function(dat, side) {
    dat %>%
      dplyr::select(dplyr::all_of(c(group_var, target_cols))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(target_cols), ~ suppressWarnings(as.numeric(.)))) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(target_cols),
                          names_to = "metric", values_to = "value") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_var)), metric, .drop = FALSE) %>%
      dplyr::summarise(
        n = dplyr::n(),
        zero_n = sum(!is.na(value) & value == 0),
        zero_rate = zero_n / n,
        .groups = "drop"
      ) %>%
      dplyr::mutate(side = side)
  }
  
  prev_rates <- calc_rate(df_prev, "prev")
  curr_rates <- calc_rate(df_curr, "curr")
  
  out <- dplyr::full_join(prev_rates, curr_rates,
                          by = c(group_var, "metric"),
                          suffix = c("_prev", "_curr")) %>%
    dplyr::mutate(
      delta_zero_rate = zero_rate_curr - zero_rate_prev,
      flag_large_delta = dplyr::if_else(!is.na(delta_zero_rate) & abs(delta_zero_rate) >= delta_threshold, 1L, 0L)
    ) %>%
    dplyr::arrange(dplyr::desc(abs(delta_zero_rate)))
  
  if (verbose) message("Zero-rate computed for ", length(target_cols), " metrics.")
  out
}
