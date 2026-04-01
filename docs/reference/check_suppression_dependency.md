# Check suppression dependency across numeric columns

Audits a data frame for suppression behavior (e.g., values represented
by "\*") across specified numeric columns. Specifically, it checks
whether suppression in a designated "trigger" column (e.g., cohort size)
is consistently applied when other numeric columns are suppressed — a
common dependency rule in public education data releases.

## Usage

``` r
check_suppression_dependency(
  df,
  numeric_cols,
  trigger_col = "cohort_students",
  return_rows = FALSE
)
```

## Arguments

- df:

  A data frame to check for suppression patterns.

- numeric_cols:

  A character vector of column names expected to contain suppressible
  numeric values (e.g., counts or rates).

- trigger_col:

  The name of the column that should control or indicate suppression
  (default is \`"cohort_students"\`).

- return_rows:

  Logical. If \`TRUE\`, the function returns suppressed row subsets in
  addition to the summary. Default is \`FALSE\`.

## Value

A tibble summarizing suppression dependencies, or a list containing the
summary and detailed row subsets if \`return_rows = TRUE\`. The summary
includes: - \`all_trigger_suppressed_in_suppressed_rows\`: Logical flag
indicating if all suppressed rows also have suppression in the trigger
column. - \`only_trigger_suppressed_rows_found\`: Count of rows where
only the trigger column is suppressed. -
\`other_suppressed_when_trigger_not\`: Count of rows where other columns
are suppressed but the trigger column is not.
