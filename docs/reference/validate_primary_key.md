# Validate primary key uniqueness in a dataset

Checks whether a set of columns uniquely identify rows in a data frame.
Can be run on a full dataset or a sampled subset to improve performance
on large data.

## Usage

``` r
validate_primary_key(
  data,
  key_cols,
  sample_n = 10000,
  full_run = FALSE,
  seed = 1234,
  show_examples = 10
)
```

## Arguments

- data:

  A data frame to check.

- key_cols:

  A character vector of column names that should act as the primary key.

- sample_n:

  Integer. Number of rows to sample for validation (if \`full_run =
  FALSE\`). Default is 10,000.

- full_run:

  Logical. If \`TRUE\`, checks the entire dataset. Default is \`FALSE\`.

- seed:

  An integer used to seed the random sample (only applies if \`full_run
  = FALSE\`). Default is 1234.

- show_examples:

  Integer. If duplicates are found, print up to this many example
  offending key combinations (after de-duplicating). Set to 0 to
  suppress examples. Default is 10.

## Value

Invisibly returns \`NULL\`. Prints a success or failure message
indicating whether the key uniquely identifies rows.

## Details

\- Columns listed in \`key_cols\` must all exist in \`data\`. - If
\`full_run = FALSE\` and the dataset has more than \`sample_n\` rows, a
sample is used. - Uses \`anyDuplicated()\` for fast duplicate
detection. - Messages use colored console output (green = valid, red =
invalid).

## Examples

``` r
validate_primary_key(mydata, key_cols = c("student_id", "year"))
#> Error: object 'mydata' not found
```
