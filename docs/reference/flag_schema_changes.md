# Flag schema changes by comparing to a prior-year archive

Compares the current schema definition against an archived schema log
from a specified year, and flags tables for rebuild if any columns have
changed in SQL type, nullability, or primary key status. Adds a
\`rebuild_flag\` column to the original schema data frame.

## Usage

``` r
flag_schema_changes(schema_df, year)
```

## Arguments

- schema_df:

  A data frame representing the current schema, typically generated from
  a table structure.

- year:

  A 4-digit year (as numeric or character) used to locate the archived
  schema log.

## Value

A copy of \`schema_df\` with an added \`rebuild_flag\` column: - \`1\`
if the table requires rebuild due to schema changes - \`0\` otherwise

## Details

\- The archived schema must exist at a standardized location on the T
drive. - Column names compared include: \`sql_type\`, \`null_flag\`, and
\`is_primary_key\`. - Uses an internal normalization step
(\`normalize_for_compare()\`) to align formats before comparison.
