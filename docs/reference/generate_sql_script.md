# Generate a T-SQL script for loading warehouse tables

Builds a full SQL script to create tables, insert data, and apply
foreign key constraints based on a flagged schema log and a prior-year
comparison. The script includes metadata tracking, expected object
checks, and runtime diagnostics for bulk loading into a SQL Server
warehouse.

## Usage

``` r
generate_sql_script(
  prior_year,
  combined_folder,
  data_source,
  schema_log_path = "T:/Data Warehouse/Warehouse Ready Files/schema_files"
)
```

## Arguments

- prior_year:

  A 4-digit year (numeric or character) used to locate the archived
  schema log for comparison.

- combined_folder:

  A string that names the folder containing flat files to be loaded
  (e.g., \`"2019_2024"\`).

- data_source:

  A string indicating the source category (\`"assessment"\`, \`"cde"\`,
  or \`"dashboard"\`).

- schema_log_path:

  Path to the folder containing schema logs. Default is the shared
  warehouse T drive path.

## Value

Invisibly returns the full SQL script as a single string. Also writes
the script to a \`.sql\` file in the \`schema_log_path\` directory with
a filename formatted as
\`\<source\>\_data_warehouse\_\<combined_folder\>.sql\`.

## Details

\- Automatically flags schema differences using
\`flag_schema_changes()\` and saves a frozen copy of the schema log. -
Assembles the full script using \`sql_table_coder()\`,
\`sql_insert_coder()\`, and \`sql_foreign_key_coder()\`. - Automatically
builds declarations for expected tables and FKs and prints progress and
summary info. - Generates SQL for error detection, transaction control,
and post-run diagnostics.
