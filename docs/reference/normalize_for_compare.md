# Normalize schema log values for comparison

Standardizes key fields in a schema metadata data frame so that
column-by-column comparisons (e.g., across years) are reliable. This
function is typically used before schema diffs or rebuild flagging.

## Usage

``` r
normalize_for_compare(df)
```

## Arguments

- df:

  A data frame containing at least the following columns: -
  \`table_name\` - \`column_name\` - \`sql_type\` - \`null_flag\` -
  \`is_primary_key\`

## Value

A data frame with the same columns but with normalized values: - Trims
whitespace - Converts SQL types like \`decimal(p,0)\` and \`int(n)\` to
\`INT\` - Converts all \`sql_type\`, \`null_flag\`, and
\`is_primary_key\` fields to uppercase

## Details

\- SQL types are uppercased and simplified for consistent matching -
NULL flags are standardized to either \`"NULL"\` or \`"NOT NULL"\` -
Primary key flags are standardized to \`"YES"\` or \`"NO"\`

## Examples

``` r
if (FALSE) { # \dontrun{
normalized <- normalize_for_compare(schema_log)
} # }
```
