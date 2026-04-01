# Validate that an old-to-new group mapping is one-to-one

Checks whether each unique value in the \`old_col\` maps to exactly one
unique value in the \`new_col\`, and vice versa. Useful for validating
recode tables, lookup joins, or classification maps before applying
them. Uses \`janitor::tabyl()\` to cross-tabulate values and ensures
each row and column contains only one match.

## Usage

``` r
validate_group_mapping_tabyl(data, old_col, new_col)
```

## Arguments

- data:

  A data frame containing the columns to be compared.

- old_col:

  The unquoted name of the column representing the original values
  (e.g., codes or old labels).

- new_col:

  The unquoted name of the column representing the new mapped values.

## Value

Invisibly returns \`TRUE\` if the mapping is one-to-one; otherwise
\`FALSE\`. Prints a success message if valid, or detailed messages if
violations are found.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_group_mapping_tabyl(data, old_code, new_label)
} # }
```
