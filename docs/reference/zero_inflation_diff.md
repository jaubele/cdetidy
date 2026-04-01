# Compare zero-value rates across two datasets

Calculates and compares the proportion of zero values in numeric (or
numeric-like) columns between two datasets (typically across years or
versions). Flags variables whose zero rates differ by a threshold,
optionally within groups.

## Usage

``` r
zero_inflation_diff(
  df_prev,
  df_curr,
  group_var = NULL,
  delta_threshold = 0.05,
  numeric_cols = NULL,
  verbose = FALSE,
  from_enc = "latin1"
)
```

## Arguments

- df_prev:

  A data frame representing the "previous" or baseline dataset.

- df_curr:

  A data frame representing the "current" or updated dataset.

- group_var:

  Optional. A character string specifying a grouping variable (e.g.,
  \`"district"\` or \`"cds"\`). If \`NULL\` (default), zero-rate
  comparison is performed at the overall level.

- delta_threshold:

  A numeric value (default \`0.05\`) representing the minimum absolute
  change in zero-rate that will be flagged.

- numeric_cols:

  Optional. A character vector of column names to limit comparison to
  specific numeric columns. If \`NULL\`, the function auto-detects
  numeric or numeric-like columns.

- verbose:

  Logical. If \`TRUE\`, prints information about which columns are being
  compared.

## Value

A data frame with one row per group-variable/metric combination,
including:

- n_prev, zero_rate_prev:

  Number of values and proportion of zero values in \`df_prev\`

- n_curr, zero_rate_curr:

  Same for \`df_curr\`

- delta_zero_rate:

  Difference in zero rates

- flag_large_delta:

  1 if the delta exceeds \`delta_threshold\`, else 0

## Examples

``` r
prev <- data.frame(a = c(0, 1, 2), b = c(0, 0, 1))
curr <- data.frame(a = c(0, 0, 0), b = c(1, 1, 1))
zero_inflation_diff(prev, curr, delta_threshold = 0.2)
#> Error in dat %>% dplyr::select(dplyr::all_of(c(group_var, target_cols))) %>%     dplyr::mutate(dplyr::across(dplyr::all_of(target_cols), ~suppressWarnings(as.numeric(.)))) %>%     tidyr::pivot_longer(cols = dplyr::all_of(target_cols), names_to = "metric",         values_to = "value") %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var)),     metric, .drop = FALSE) %>% dplyr::summarise(n = dplyr::n(),     zero_n = sum(!is.na(value) & value == 0), zero_rate = zero_n/n,     .groups = "drop") %>% dplyr::mutate(side = side): could not find function "%>%"
```
