# Identify and resolve conflicting CDS codes in a dataset

Wraps \`get_conflicting_cds()\` and \`replace_conflicting_cds()\` to
identify duplicate CDS codes that span multiple organization levels and
resolve them by appending a suffix (e.g., \`"9999"\`) to the
school-level (\`"S"\`) CDS codes. The resulting dataset preserves
uniqueness while maintaining traceability.

## Usage

``` r
resolve_conflicting_cds(df, cds_col, org_level_col)
```

## Arguments

- df:

  A data frame containing CDS codes and organization level indicators.

- cds_col:

  A string specifying the column name containing the CDS code.

- org_level_col:

  A string specifying the column name containing the organization level
  (e.g., \`"C"\`, \`"D"\`, \`"S"\`).

## Value

A modified data frame with resolved CDS values. A \`"conflicting_cds"\`
attribute is attached to the returned data frame, containing the list of
detected conflicts.
