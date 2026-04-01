# Safely write a warehouse-ready CSV file with metadata logging

Exports a data frame to a standardized warehouse folder structure (e.g.,
T drive), ensures correct character formatting for ID fields, applies
optional compression, and logs relevant metadata to a centralized export
log. The function enforces expected naming and structural conventions
for OCDE’s data warehouse and supports both fact and dimension tables.

## Usage

``` r
safe_fwrite_warehouse(
  data,
  path = NULL,
  char_cols = c("cds", "county_code", "district_code", "school_code"),
  compress = FALSE,
  n_check = 6,
  log_metadata = NULL,
  data_year = NULL,
  data_source = NULL,
  data_description = NA,
  user_note = NA,
  table_name = NULL,
  dim_description = NULL,
  log_path = "export_log.csv",
  canonical_table_id = NULL,
  dimension_type = NULL
)
```

## Arguments

- data:

  A data frame to be exported.

- path:

  Optional. A full file path to write the CSV file. If \`NULL\`, a
  default path is constructed using metadata fields.

- char_cols:

  A character vector of column names to force as character before
  export. Defaults to key ID fields (\`cds\`, \`county_code\`, etc.).

- compress:

  Logical. If \`TRUE\`, writes the file as a gzipped \`.csv.gz\`.
  Default is \`FALSE\`.

- n_check:

  Number of rows to preview (with \`fread()\`) after writing. Default is
  6.

- log_metadata:

  Optional. A named list containing key metadata: \`data_year\`,
  \`data_source\`, \`data_description\`, \`user_note\`.

- data_year:

  Data year (numeric or character) — required if \`log_metadata\` is not
  supplied.

- data_source:

  A string describing the data source (e.g., \`"CDE"\`,
  \`"Dashboard"\`).

- data_description:

  A short description of the dataset (e.g., \`"Absenteeism summary by
  subgroup"\`).

- user_note:

  A comment describing the export context (must include \`"fact"\` or
  \`"dim"\` to infer table type).

- table_name:

  Base name for the output table.

- dim_description:

  Optional. A short, clean label for the dimension table (used in the
  file name).

- log_path:

  Path to the export log CSV file. Default is \`"export_log.csv"\`
  within the warehouse structure.

- canonical_table_id:

  Optional. A unique identifier for the export record. Defaults to
  \`table_name\`.

- dimension_type:

  Optional. Type of dimension table (\`"universal"\`, \`"annualized"\`,
  or \`"other"\`).

## Value

Invisibly returns \`NULL\`. The function writes the CSV and appends (or
updates) a log entry.

## Details

\- Constructs a file name of the form:
\`\<table_name\>\_\<dim_description\>\_\<fact\|dim\>.csv\` - Ensures ID
columns (e.g., \`cds\`) are properly coerced to character -
Automatically creates subdirectories if they don't exist - Overwrites an
existing log entry if the \`canonical_table_id\` matches

## See also

\[safe_fwrite()\] for the general version that is not
warehouse-specific.
