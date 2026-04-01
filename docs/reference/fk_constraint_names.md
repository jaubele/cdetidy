# Generate deterministic foreign key constraint names from schema metadata

Constructs constraint names for foreign keys based on a flagged schema
data frame, handling both single-column and multi-column (composite)
foreign key relationships. Returns a character vector of cleaned and
deterministic names suitable for use in SQL table creation scripts.

## Usage

``` r
fk_constraint_names(schema_flagged)
```

## Arguments

- schema_flagged:

  A data frame containing schema metadata, with columns: -
  \`table_name\`: name of the table - \`column_name\`: name of the
  column - \`is_primary_key\`: "YES" or "NO" for PK status -
  \`is_foreign_key\`: "YES" or "NO" for FK status - \`foreign_key_ref\`:
  reference target(s), formatted like \`"table(col)"\` or
  \`"table1(col1) \| table2(col2)"\`

## Value

A character vector of foreign key constraint names. Each name follows
the format: - For single-column FKs:
\`fk\_\<child_table\>\_\<child_column\>\_\<ref_table\>\` - For
multi-column composite FKs:
\`fk\_\<child_table\>\_\<ref_table\>\_\<ordered_child_cols\>\`

## Details

\- Constraint names are made SQL-safe by replacing invalid characters
with underscores. - Composite constraints are only generated if the
referenced columns match the full PK of the referenced table. -
Duplicate or malformed references will result in an error.

## Examples

``` r
if (FALSE) { # \dontrun{
fk_constraint_names(schema_flagged_df)
} # }
```
