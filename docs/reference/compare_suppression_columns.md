# Compare suppression-affected columns across two datasets

Uses the \`star_scan()\` utility to detect which columns contain
suppressed values (typically marked by \`"\*"\`), and compares two data
frames to identify changes in suppression status across columns.

## Usage

``` r
compare_suppression_columns(df_prev, df_curr)
```

## Arguments

- df_prev:

  A data frame representing the earlier or baseline dataset.

- df_curr:

  A data frame representing the newer or updated dataset.

## Value

A tibble with three columns:

- new_suppressed_cols:

  A list of column names that are newly suppressed in \`df_curr\` but
  not in \`df_prev\`

- stopped_being_suppressed:

  Columns that were suppressed in \`df_prev\` but are no longer
  suppressed in \`df_curr\`

- unchanged_suppressed_cols:

  Columns that are suppressed in both datasets

## Details

This function relies on the \`star_scan()\` function being available in
the environment. By default, it looks for \`"\*"\` values as indicators
of suppression. This is useful for QA comparisons of public CDE files
across time or versions.

## See also

\[star_scan()\]

## Examples

``` r
df1 <- data.frame(a = c(1, 2), b = c("*", 4), c = c("*", "*"))
df2 <- data.frame(a = c("*", "*"), b = c("3", "4"), c = c("*", "*"))
compare_suppression_columns(df1, df2)
#> Error in map_lgl(x, ~any(grepl(pattern, as.character(.x), fixed = fixed,     useBytes = TRUE), na.rm = TRUE)): could not find function "map_lgl"
```
