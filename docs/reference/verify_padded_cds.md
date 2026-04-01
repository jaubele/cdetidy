# Verify that CDS and component columns are correctly padded

Checks whether CDS-related columns (\`county_code\`, \`district_code\`,
\`school_code\`, and \`cds\`) are character type and have the expected
number of digits (2, 5, 7, and 14, respectively). Optionally drops the
component columns after successful validation.

## Usage

``` r
verify_padded_cds(df, drop_parts = TRUE)
```

## Arguments

- df:

  A data frame containing the CDS and its component columns.

- drop_parts:

  Logical. If \`TRUE\`, removes \`county_code\`, \`district_code\`, and
  \`school_code\` after successful validation. Default is \`TRUE\`.

## Value

Returns the input data frame (possibly with columns dropped). Returns
\`FALSE\` (invisibly) if validation fails. Messages are printed using
the \`cli\` package.

## Details

\- Each column must be present, of type character, and match its
expected width. - The expected lengths are: - \`county_code\`: 2
digits - \`district_code\`: 5 digits - \`school_code\`: 7 digits -
\`cds\`: 14 digits

## Examples

``` r
df <- data.frame(
  county_code = "01",
  district_code = "12345",
  school_code = "6789012",
  cds = "01123456789012",
  stringsAsFactors = FALSE
)
verify_padded_cds(df)
#> Error in purrr::imap(expected_lengths, function(width, col) {    if (!col %in% names(df)) {        return(glue::glue("❌ Column '{col}' is missing."))    }    values <- df[[col]]    if (!is.character(values)) {        return(glue::glue("❌ Column '{col}' is not character (is {typeof(values)})."))    }    bad_n <- sum(nchar(values) != width, na.rm = TRUE)    if (bad_n > 0) {        return(glue::glue("⚠️ Column '{col}' has {bad_n} rows with incorrect length (expected {width})."))    }    NULL}) %>% purrr::compact(): could not find function "%>%"
```
