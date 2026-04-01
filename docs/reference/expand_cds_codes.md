# Expand a full CDS code into county, district, and school components

Splits a 14-character CDS code into its component parts: county (2
digits), district (5 digits), and school (7 digits). Pads each component
to ensure correct width using leading zeros.

## Usage

``` r
expand_cds_codes(
  df,
  cds_col = "cds",
  county_col = "county_code",
  district_col = "district_code",
  school_col = "school_code"
)
```

## Arguments

- df:

  A data frame containing a column with full CDS codes.

- cds_col:

  The name of the CDS column. Default is \`"cds"\`.

- county_col:

  The name of the column to store the county code. Default is
  \`"county_code"\`.

- district_col:

  The name of the column to store the district code. Default is
  \`"district_code"\`.

- school_col:

  The name of the column to store the school code. Default is
  \`"school_code"\`.

## Value

A data frame with new columns for county, district, and school codes.

## Examples

``` r
df <- data.frame(cds = "01023000400567")
expand_cds_codes(df)
#> Error in df %>% mutate(`:=`(!!county_col, str_pad(str_sub(.data[[cds_col]],     1, 2), 2, pad = "0")), `:=`(!!district_col, str_pad(str_sub(.data[[cds_col]],     3, 7), 5, pad = "0")), `:=`(!!school_col, str_pad(str_sub(.data[[cds_col]],     8, 14), 7, pad = "0"))): could not find function "%>%"
```
