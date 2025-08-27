#' Pad county, district, and school codes and optionally generate CDS
#'
#' Ensures that `county_code`, `district_code`, and `school_code` are left-padded with zeros
#' to their standard widths (2, 5, and 7 digits respectively). Optionally, generates a full
#' CDS code by concatenating the padded columns.
#'
#' @param df A data frame containing columns for county, district, and school codes.
#' @param county_col The name of the county code column. Default is `"county_code"`.
#' @param district_col The name of the district code column. Default is `"district_code"`.
#' @param school_col The name of the school code column. Default is `"school_code"`.
#' @param create_cds Logical. If `TRUE`, creates a new CDS column by concatenating the three parts. Default is `TRUE`.
#' @param cds_col The name of the output CDS column if `create_cds = TRUE`. Default is `"cds"`.
#'
#' @return A data frame with padded code columns and, if requested, a new CDS column.
#'
#' @examples
#' df <- data.frame(county_code = 1, district_code = 23, school_code = 456)
#' pad_cds_codes(df)
#'
#' @export

pad_cds_codes <- function(df,
                          county_col = "county_code",
                          district_col = "district_code",
                          school_col = "school_code",
                          create_cds = TRUE,
                          cds_col = "cds") {
  df <- df %>%
    mutate(
      !!county_col   := str_pad(as.character(.data[[county_col]]),   width = 2, side = "left", pad = "0"),
      !!district_col := str_pad(as.character(.data[[district_col]]), width = 5, side = "left", pad = "0"),
      !!school_col   := str_pad(as.character(.data[[school_col]]),   width = 7, side = "left", pad = "0")
    )

  if (create_cds) {
    df <- df %>%
      mutate(
        !!cds_col := paste0(.data[[county_col]], .data[[district_col]], .data[[school_col]])
      )
  }

  return(df)
}

# For Dashboard files, use this function to separate CDS code into county, district, school codes


