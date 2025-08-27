#' Expand a full CDS code into county, district, and school components
#'
#' Splits a 14-character CDS code into its component parts:
#' county (2 digits), district (5 digits), and school (7 digits).
#' Pads each component to ensure correct width using leading zeros.
#'
#' @param df A data frame containing a column with full CDS codes.
#' @param cds_col The name of the CDS column. Default is `"cds"`.
#' @param county_col The name of the column to store the county code. Default is `"county_code"`.
#' @param district_col The name of the column to store the district code. Default is `"district_code"`.
#' @param school_col The name of the column to store the school code. Default is `"school_code"`.
#'
#' @return A data frame with new columns for county, district, and school codes.
#'
#' @examples
#' df <- data.frame(cds = "01023000400567")
#' expand_cds_codes(df)
#'
#' @export

expand_cds_codes <- function(df,
                             cds_col = "cds",
                             county_col = "county_code",
                             district_col = "district_code",
                             school_col = "school_code") {
  df <- df %>%
    mutate(
      !!county_col   := str_pad(str_sub(.data[[cds_col]], 1, 2), 2, pad = "0"),
      !!district_col := str_pad(str_sub(.data[[cds_col]], 3, 7), 5, pad = "0"),
      !!school_col   := str_pad(str_sub(.data[[cds_col]], 8, 14), 7, pad = "0")
    )
  
  return(df)
}