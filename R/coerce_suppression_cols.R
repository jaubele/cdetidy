#' Coerce columns with suppression characters into numeric or integer
#' 
#' Should be used just after star_scan function
#'
#' Converts one or more columns that contain suppression markers (e.g., `"*"`)
#' into numeric or integer values. Suppressed values are treated as `NA`. This
#' function attempts to intelligently convert values to `integer` if they appear
#' to be whole numbers, unless `prefer_integer_when_whole = FALSE`.
#'
#' @param df A data frame containing the columns to coerce.
#' @param cols A character vector of column names to coerce.
#' @param prefer_integer_when_whole Logical. If `TRUE` (default), columns that
#'   contain only whole numbers (aside from `NA`) will be coerced to `integer`.
#'   Otherwise, they will remain as `numeric`.
#' @param wipe_problems_attr Logical. If `TRUE` (default), removes any lingering
#'   `problems` attributes left behind by `readr::parse_number()`.
#' @param wipe_scope Either `"all"` (default) or `"selected"`. If `"all"`, removes
#'   the `problems` attribute from all columns. If `"selected"`, only the columns
#'   specified in `cols` will be cleaned.
#'
#' @return The original data frame with selected columns coerced to numeric or integer
#' types and optionally cleaned of `problems` attributes.
#'
#' @examples
#' grad19 <- coerce_suppression_cols(grad19, suppress_cols)
#'
#' @export

coerce_suppression_cols <- function(df,
                                    cols,
                                    prefer_integer_when_whole = TRUE,
                                    wipe_problems_attr = TRUE,
                                    wipe_scope = c("all", "selected")) {
  if (length(cols) == 0) return(df)
  wipe_scope <- match.arg(wipe_scope)
  
  clean_and_cast <- function(x) {
    # Detect decimals (pre-parse for characters; post-parse for numerics)
    has_decimal <-
      if (is.numeric(x)) {
        any(!is.na(x) & (x != floor(x)))
      } else {
        any(grepl("\\.", x, useBytes = TRUE), na.rm = TRUE)
      }
    
    # Parse: treat "*" as NA to avoid attaching a "problems" attribute
    num <- if (is.numeric(x)) x else suppressWarnings(readr::parse_number(x, na = "*"))
    
    # Decide integer vs numeric
    if (!has_decimal) {
      if (!prefer_integer_when_whole) return(num)
      whole <- !is.na(num) & (num == floor(num))
      if (all(whole | is.na(num))) return(as.integer(num))
      return(num)
    } else {
      if (prefer_integer_when_whole) {
        whole <- !is.na(num) & (num == floor(num))
        if (all(whole | is.na(num))) return(as.integer(num))
      }
      return(num)
    }
  }
  
  df <- df %>% mutate(across(all_of(cols), clean_and_cast))
  
  # Optional: wipe any lingering readr "problems" attributes
  if (wipe_problems_attr) {
    if (wipe_scope == "all") {
      df <- df %>% mutate(across(everything(), ~ { attr(.x, "problems") <- NULL; .x }))
    } else {
      df <- df %>% mutate(across(all_of(cols), ~ { attr(.x, "problems") <- NULL; .x }))
    }
  }
  
  df
}