# Generate SQL scripts to add foreign key constraints

Based on a schema metadata data frame (e.g., from \`sql_schema_log()\`),
this function generates T-SQL \`ALTER TABLE\` statements to define
foreign key relationships. It supports both single-column and composite
key relationships and avoids duplicate constraint creation using \`IF
NOT EXISTS\` checks. The output is suitable for use in post-load SQL
warehouse scripts.

## Usage

``` r
sql_foreign_key_coder(schema_flagged)
```

## Arguments

- schema_flagged:

  A data frame containing schema metadata, including the following
  columns: - \`table_name\`: Name of the child table - \`column_name\`:
  Foreign key column in the child table - \`is_foreign_key\`: Logical or
  "YES"/"NO" string - \`foreign_key_ref\`: Reference in
  \`"table(column)"\` or \`"table1(col1) \| table2(col2)"\` format -
  \`is_primary_key\`: Indicates primary key status for composite
  detection

## Value

A character vector of SQL \`ALTER TABLE\` statements to define foreign
key constraints. Each statement begins with an \`IF NOT EXISTS\` check
to avoid duplicate creation.

## Details

\- Composite keys are supported if the referenced columns match the full
PK of the referenced table. - Names are cleaned for SQL compatibility
using safe alphanumeric formatting. - The function uses \`glue::glue()\`
for string assembly and assumes the presence of \`sql_ident()\` for
quoting identifiers.
