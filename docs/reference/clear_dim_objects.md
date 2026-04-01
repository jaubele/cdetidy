# Remove dimension-related objects from the global environment

Scans the global environment for objects with names that begin with a
user-defined \`base_string\` and contain keywords like "dim", "entit",
"group", or "label". It optionally removes them or just displays them in
a dry run. Useful for cleaning up intermediate or output objects created
during ETL or report generation.

## Usage

``` r
clear_dim_objects(
  base_string = NULL,
  dry_run = TRUE,
  keywords = c("dim", "entit", "group", "label")
)
```

## Arguments

- base_string:

  A string that all targeted object names should begin with (e.g.,
  \`"ltel19"\`, \`"sbac24"\`).

- dry_run:

  Logical. If \`TRUE\`, shows which objects would be removed without
  actually deleting them. Default is \`TRUE\`.

- keywords:

  A character vector of substrings to match within object names.
  Defaults to \`c("dim", "entit", "group", "label")\`.

## Value

(Invisibly) A character vector of object names that matched the filters.
If \`dry_run = FALSE\`, these objects are removed.

## Details

\- This function uses pattern matching to find "dimension-like"
objects. - It searches for objects in the global environment
(\`.GlobalEnv\`). - Useful for scripts that generate many intermediate
tables for reporting or dashboarding.
