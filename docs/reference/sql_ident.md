# Safely quote SQL Server identifiers

Wraps a character string in square brackets for use as a SQL Server
identifier. Escapes any closing brackets within the string to avoid
syntax errors. Useful for safely referencing table or column names in
dynamic SQL generation.

## Usage

``` r
sql_ident(x)
```

## Arguments

- x:

  A character vector of table or column names.

## Value

A character vector with each element wrapped in square brackets and
escaped properly.

## Examples

``` r
sql_ident("table")           # "[table]"
#> [1] "[table]"
sql_ident("column name")     # "[column name]"
#> [1] "[column name]"
sql_ident("some]name")       # "[some]]name]"
#> [1] "[some]]name]"
```
