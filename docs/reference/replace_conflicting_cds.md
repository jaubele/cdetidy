# Resolve conflicting CDS codes by appending a suffix to school-level entries

Detects CDS codes that appear at multiple organization levels and
appends \`"9999"\` to the \`cds\` code for school-level (\`"S"\`)
records, allowing the dataset to preserve uniqueness. This is typically
used to resolve ambiguity in CDE datasets before loading into a
warehouse.

## Usage

``` r
replace_conflicting_cds(
  df,
  conflicting_cds_df,
  cds_col = "cds",
  org_level_col = "org_level"
)
```

## Arguments

- df:

  A data frame containing the CDS column and org level column to modify.

- conflicting_cds_df:

  A data frame containing the list of conflicting CDS codes, typically
  returned by \`get_conflicting_cds()\`.

- cds_col:

  The name of the column that contains the CDS code. Default is
  \`"cds"\`.

- org_level_col:

  The name of the column that contains the org level. Default is
  \`"org_level"\`.

## Value

A data frame with updated CDS values. Adds a temporary \`altered_cds\`
column that marks modified rows with \`1\`.
