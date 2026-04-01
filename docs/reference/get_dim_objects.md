# List dimension-like objects in the global environment

Searches the global environment for object names that contain
dimension-related keywords such as \`"dim"\`, \`"entit"\`, \`"group"\`,
or \`"label"\`, and returns them (excluding known function names).

## Usage

``` r
get_dim_objects(keywords = c("dim", "entit", "group", "label"))
```

## Arguments

- keywords:

  A character vector of keywords to search for within object names.
  Default includes \`"dim"\`, \`"entit"\`, \`"group"\`, and \`"label"\`.

## Value

Invisibly returns a character vector of matching object names. Also
prints a styled summary using \`cli\`.

## Details

\- Objects are matched using a keyword-based regular expression. - Known
function names are excluded from the results. - This function is useful
for interactively auditing which dimension-like data frames exist in
memory.
