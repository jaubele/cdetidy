# Compare column names between two data frames

Compares the column names of two data frames and prints a summary of
whether they are identical, along with the specific variables that are
present in one but not the other. This function is useful for checking
schema consistency across datasets, especially in ETL or QA workflows.

## Usage

``` r
compare_variable_names(df1, df2)
```

## Arguments

- df1:

  The first data frame to compare.

- df2:

  The second data frame to compare.

## Value

A tibble summarizing the comparison (indicating whether the columns are
identical), and printed messages that describe which columns are unique
to each data frame. No object is returned explicitly.

## Examples

``` r
df1 <- data.frame(a = 1, b = 2)
df2 <- data.frame(a = 1, c = 3)
compare_variable_names(df1, df2)
#> Error in tibble(Comparison = paste(name1, "vs", name2), Identical = identical_cols): could not find function "tibble"
```
