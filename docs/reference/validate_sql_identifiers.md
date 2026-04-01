# Validate SQL-safe table or column names

Checks that each name in a character vector follows SQL identifier
rules: - Starts with a letter or underscore - Contains only letters,
numbers, or underscores - Does not exceed a specified maximum length

## Usage

``` r
validate_sql_identifiers(
  name_vec,
  type = c("column", "table"),
  max_length = 128
)
```

## Arguments

- name_vec:

  A character vector of names to validate.

- type:

  A string indicating what type of identifier is being checked. Must be
  \`"column"\` or \`"table"\`. Default is \`"column"\`.

- max_length:

  Maximum number of characters allowed in a name. Default is 128.

## Value

Invisibly returns \`NULL\`. Raises an error if any invalid names are
found.

## Details

\- This function enforces safe naming for SQL Server and
PostgreSQL-style identifiers. - Colored error messages are printed for
easier debugging in interactive sessions.

## Examples

``` r
validate_sql_identifiers(c("student_id", "_school_code"))  # ✅ valid
validate_sql_identifiers(c("123bad", "with space"), type = "column")  # ❌ will error
#> Error in validate_sql_identifiers(c("123bad", "with space"), type = "column"): ❌ Invalid SQL column name(s): 123bad, with space
#> ⚠️ Must start with a letter or underscore, and contain only letters, numbers, or underscores.
```
