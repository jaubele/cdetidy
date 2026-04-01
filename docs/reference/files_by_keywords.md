# Search for and select files by keyword from a folder

Searches a specified folder for filenames that match any of the given
keywords, then presents an interactive menu for the user to select files
to return.

## Usage

``` r
files_by_keywords(
  folder,
  keywords,
  recursive = TRUE,
  ignore_case = TRUE,
  full_names = TRUE
)
```

## Arguments

- folder:

  A string path to the folder where files will be searched.

- keywords:

  A character vector of one or more keywords to match against filenames.

- recursive:

  Logical. If \`TRUE\`, search subdirectories as well. Default is
  \`TRUE\`.

- ignore_case:

  Logical. If \`TRUE\`, matching is case-insensitive. Default is
  \`TRUE\`.

- full_names:

  Logical. If \`TRUE\`, returns full paths; otherwise, returns filenames
  only. Default is \`TRUE\`.

## Value

A character vector of selected file paths. If the user cancels or no
files match, an empty character vector is returned.

## Details

\- The function filters files based on whether any keyword appears in
the filename (not the full path). - If one or more files match, the user
is prompted to select files by number. - If no keywords match or the
user cancels, nothing is returned.
