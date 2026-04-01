# Generate and log a SQL schema for a data frame

Builds a schema definition table from a data frame by inspecting its
column types, nullability, and primary/foreign key indicators. The
schema is saved to a central log (\`primary_sql_schema_log.csv\`) and to
an individual table-level schema file.

## Usage

``` r
sql_schema_log_warehouse(
  data,
  table_name,
  data_year,
  data_source,
  user_note = NA,
  dim_description = NULL,
  max_char = 255,
  primary_key = NULL,
  foreign_keys = NULL,
  canonical_table_id = NULL,
  dimension_type = NULL
)
```

## Arguments

- data:

  A data frame to analyze.

- table_name:

  The base name of the SQL table.

- data_year:

  A 4-digit year indicating the data vintage.

- data_source:

  A string identifying the data source (must be \`"Assessment"\`,
  \`"CDE"\`, or \`"Dashboard"\`).

- user_note:

  A short note describing the data (must include \`"fact"\` or
  \`"dim"\`).

- dim_description:

  Optional. A short label describing the dimension content (e.g.,
  \`"race_ethnicity"\`).

- max_char:

  Maximum length to assign to character columns (default is 255).

- primary_key:

  A character vector of column(s) that make up the primary key.

- foreign_keys:

  A named character vector like \`c("column_name" = "ref_table")\`.

- canonical_table_id:

  Optional. A unique ID for the table. Defaults to \`table_name\`.

- dimension_type:

  Optional. One of \`"universal"\`, \`"annualized"\`, or \`"other"\`
  (used only for dimension tables).

## Value

A tibble containing the schema definition for the input data. The result
is also written to: - The central schema log: \`T:/Data
Warehouse/schema_files/primary_sql_schema_log.csv\` - An individual
schema file by table: \`T:/Data
Warehouse/schema_files/\<year\>/\<source\>/\<fact\|dim\>/\<table\>\_schema.csv\`

## Details

\- Infers SQL column types (\`VARCHAR\`, \`INT\`, \`DECIMAL\`, etc.)
based on data values. - Enforces uniqueness and completeness of primary
key. - Validates format and existence of foreign key mappings. - Updates
the central log by replacing existing entries for the same
\`canonical_table_id\`.
