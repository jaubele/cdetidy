# Filter a schema log for a specific data source (with optional universal tables)

Filters a schema metadata table (typically from
\`primary_sql_schema_log.csv\`) to include only rows that match a
specified \`data_source\`, optionally including "universal" dimensions.

## Usage

``` r
filter_schema_sources(schema_df, data_source = NULL, include_universal = TRUE)
```

## Arguments

- schema_df:

  A data frame representing the full schema log, including columns
  \`data_source\`, \`dimension_type\`, and \`table_name\`.

- data_source:

  A string or character vector specifying one or more of
  \`"assessment"\`, \`"cde"\`, or \`"dashboard"\`. Required.

- include_universal:

  Logical. If \`TRUE\`, also includes rows where \`dimension_type ==
  "universal"\`. Default is \`TRUE\`.

## Value

A filtered data frame containing only schema rows matching the specified
data source(s), plus universal tables if \`include_universal = TRUE\`.

## Details

\- The function normalizes \`data_source\` labels to match the schema
log. - It will error if no matching rows are found or if an invalid
\`data_source\` is provided. - Useful for generating SQL, inserts, or
table builds scoped to a single source system.

## Examples

``` r
if (FALSE) { # \dontrun{
schema_log <- readr::read_csv("primary_sql_schema_log.csv")
filtered <- filter_schema_sources(schema_log, data_source = "assessment")
} # }
```
