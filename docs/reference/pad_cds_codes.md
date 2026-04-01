# Pad county, district, and school codes and optionally generate CDS

Ensures that \`county_code\`, \`district_code\`, and \`school_code\` are
left-padded with zeros to their standard widths (2, 5, and 7 digits
respectively). Optionally, generates a full CDS code by concatenating
the padded columns.

## Usage

``` r
pad_cds_codes(
  df,
  county_col = "county_code",
  district_col = "district_code",
  school_col = "school_code",
  create_cds = TRUE,
  cds_col = "cds"
)
```

## Arguments

- df:

  A data frame containing columns for county, district, and school
  codes.

- county_col:

  The name of the county code column. Default is \`"county_code"\`.

- district_col:

  The name of the district code column. Default is \`"district_code"\`.

- school_col:

  The name of the school code column. Default is \`"school_code"\`.

- create_cds:

  Logical. If \`TRUE\`, creates a new CDS column by concatenating the
  three parts. Default is \`TRUE\`.

- cds_col:

  The name of the output CDS column if \`create_cds = TRUE\`. Default is
  \`"cds"\`.

## Value

A data frame with padded code columns and, if requested, a new CDS
column.

## Examples

``` r
df <- data.frame(county_code = 1, district_code = 23, school_code = 456)
pad_cds_codes(df)
#> Error in df %>% mutate(`:=`(!!county_col, str_pad(as.character(.data[[county_col]]),     width = 2, side = "left", pad = "0")), `:=`(!!district_col,     str_pad(as.character(.data[[district_col]]), width = 5, side = "left",         pad = "0")), `:=`(!!school_col, str_pad(as.character(.data[[school_col]]),     width = 7, side = "left", pad = "0"))): could not find function "%>%"
```
