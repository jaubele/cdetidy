# Archive the primary SQL schema log for a given year

Copies the current \`primary_sql_schema_log.csv\` from the live folder
to an archive folder with the year included in the filename. Prevents
overwriting unless explicitly allowed.

## Usage

``` r
archive_schema_log(year, overwrite = FALSE)
```

## Arguments

- year:

  A 4-digit year as a numeric or character string. Used to name the
  archived file.

- overwrite:

  Logical. If TRUE, allows overwriting an existing archive file. Default
  is FALSE.

## Value

(Invisibly) the path to the archived schema log file as a character
string.
