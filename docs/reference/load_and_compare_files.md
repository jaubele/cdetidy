# Load, standardize ID codes, compare schemas, and optionally join files

Searches \`folder\` for files whose names match \`keywords\`, reads them
with \`readr::read_csv()\`, \*\*standardizes ID code columns\*\*,
normalizes text to UTF-8, compares column sets across files, and
(optionally) joins them using \`bind_rows\`, \`left_join\`, or
\`full_join\`.

## Usage

``` r
load_and_compare_files(
  folder,
  keywords,
  join_method = c("bind_rows", "left_join", "full_join"),
  by = NULL,
  recursive = TRUE,
  ignore_case = TRUE,
  full_names = TRUE,
  require_approval_on_diff = TRUE,
  force = FALSE,
  encoding = NULL,
  id_col = "source_file",
  show_compare_details = TRUE,
  also_run_compare_function = FALSE
)
```

## Arguments

- folder:

  Path to the folder to search.

- keywords:

  Character vector of filename keywords to match.

- join_method:

  Join strategy: one of \`"bind_rows"\` (default), \`"left_join"\`, or
  \`"full_join"\`.

- by:

  Character vector of join keys (required when \`join_method\` is not
  \`"bind_rows"\`).

- recursive:

  Logical; search subdirectories? Default \`TRUE\`.

- ignore_case:

  Logical; case-insensitive filename matching? Default \`TRUE\`.

- full_names:

  Logical; return full file paths? Default \`TRUE\`.

- require_approval_on_diff:

  Logical; when \`TRUE\`, if column name/order mismatches are found
  you’re prompted to proceed. Default \`TRUE\`.

- force:

  Logical; skip the prompt and proceed even when differences are
  detected. Default \`FALSE\`.

- encoding:

  Optional file encoding passed to \`readr::locale()\` (e.g.,
  \`"UTF-8"\`, \`"latin1"\`). Default \`"UTF-8"\`.

- id_col:

  Optional; when using \`bind_rows\`, add a column with the source
  filename. Set to \`NULL\` to omit.

- show_compare_details:

  Logical; print per-file column comparisons. Default \`TRUE\`.

- also_run_compare_function:

  Logical; if \`TRUE\` and a function named \`compare_variable_names()\`
  exists, it will be called for each file pair.

## Value

A data frame produced by joining the selected files (or \`NULL\` if
discrepancies were found and approval was denied). When \`bind_rows\` is
used with \`id_col\`, the result includes that source column.

## Details

\*\*Standardization of ID codes (idempotent; runs only on columns that
exist):\*\* - \`county_code\` → 2 chars, left-padded with \`0\`
(non-digits removed first). - \`district_code\` → 5 chars, left-padded
with \`0\`. - \`school_code\` → 7 chars, left-padded with \`0\`. -
\`cds\` → 14 chars; if missing or not 14 digits, it is rebuilt as
\`paste0(county_code, district_code, school_code)\` when those parts
exist, then left-padded to 14.

Additional behaviors: - Forces the ID columns above to \*\*character\*\*
type on read when present. - Normalizes all character columns to UTF-8
via \`iconv\`. - Compares column names (and order) across files and
reports differences. - If \`require_approval_on_diff = TRUE\` (default)
and differences are found, the function prompts before continuing
(unless \`force = TRUE\`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple row-bind with source column
out <- load_and_compare_files(
  folder = "T:/Data Warehouse/Assessment/dim",
  keywords = "school",
  join_method = "bind_rows",
  id_col = "source_file"
)

# Full join on keys after schema check
out2 <- load_and_compare_files(
  folder = "path/to/folder",
  keywords = c("part1", "part2"),
  join_method = "full_join",
  by = c("cds", "year"),
  force = TRUE
)
} # }
```
