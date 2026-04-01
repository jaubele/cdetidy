# Identify CDS codes linked to multiple organization levels

Flags CDS codes that are associated with more than one organization
level (e.g., school, district, county) within the same dataset — a
common data integrity issue when merging or aggregating across org
levels.

## Usage

``` r
get_conflicting_cds(df, cds_col = "cds", org_level_col = "org_level")
```

## Arguments

- df:

  A data frame containing at least two columns: a CDS code and an
  organization level.

- cds_col:

  A string indicating the column name that contains the CDS code.
  Default is \`"cds"\`.

- org_level_col:

  A string indicating the column name that contains the organization
  level. Default is \`"org_level"\`.

## Value

A data frame with one row per conflicting CDS code and a column
\`unique_org_levels\` indicating how many org levels were found for each
code.
