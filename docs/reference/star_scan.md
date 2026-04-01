# Scan a data frame for pattern-matching values (e.g., "\*")

This function scans all columns in a data frame and identifies which
columns contain values matching a given pattern (e.g., \`"\*"\` for
suppression). It returns a list with the names of the columns containing
matches and a summary of how many matches were found in each.

## Usage

``` r
star_scan(df, pattern = "*", fixed = TRUE, print = TRUE)
```

## Arguments

- df:

  A data frame to scan.

- pattern:

  A character string containing the pattern to match. Default is
  \`"\*"\`.

- fixed:

  Logical. If TRUE (default), \`pattern\` is treated as a fixed string.
  If FALSE, it's treated as a regular expression.

- print:

  Logical. If TRUE (default), prints a message showing which columns
  matched.

## Value

A list with two elements:

- columns:

  A character vector of column names containing matches.

- summary:

  A tibble summarizing how many matches were found per column.

## Examples

``` r
suppress_cols <- star_scan(chronic24)$columns 
#> Error in map_lgl(x, ~any(grepl(pattern, as.character(.x), fixed = fixed,     useBytes = TRUE), na.rm = TRUE)): could not find function "map_lgl"
```
