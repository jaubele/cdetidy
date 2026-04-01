# Coerce columns with suppression characters into numeric or integer

Should be used just after star_scan function

## Usage

``` r
coerce_suppression_cols(
  df,
  cols,
  prefer_integer_when_whole = TRUE,
  wipe_problems_attr = TRUE,
  wipe_scope = c("all", "selected")
)
```

## Arguments

- df:

  A data frame containing the columns to coerce.

- cols:

  A character vector of column names to coerce.

- prefer_integer_when_whole:

  Logical. If \`TRUE\` (default), columns that contain only whole
  numbers (aside from \`NA\`) will be coerced to \`integer\`. Otherwise,
  they will remain as \`numeric\`.

- wipe_problems_attr:

  Logical. If \`TRUE\` (default), removes any lingering \`problems\`
  attributes left behind by \`readr::parse_number()\`.

- wipe_scope:

  Either \`"all"\` (default) or \`"selected"\`. If \`"all"\`, removes
  the \`problems\` attribute from all columns. If \`"selected"\`, only
  the columns specified in \`cols\` will be cleaned.

## Value

The original data frame with selected columns coerced to numeric or
integer types and optionally cleaned of \`problems\` attributes.

## Details

Converts one or more columns that contain suppression markers (e.g.,
\`"\*"\`) into numeric or integer values. Suppressed values are treated
as \`NA\`. This function attempts to intelligently convert values to
\`integer\` if they appear to be whole numbers, unless
\`prefer_integer_when_whole = FALSE\`.

## Examples

``` r
grad19 <- coerce_suppression_cols(grad19, suppress_cols)
#> Error: object 'suppress_cols' not found
```
