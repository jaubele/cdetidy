# Apply classification mapping to CDE-style file columns

Applies a hardcoded classification map to one or more columns in a CDE
dataset. For each input column, the function generates new output
columns using user-defined prefixes: \`\<prefix\>\_label\`,
\`\<prefix\>\_num\`, \`\<prefix\>\_group_num\`, and
\`\<prefix\>\_group\`. Interactive prompts are used to resolve
classification values that appear in multiple entries.

## Usage

``` r
cde_files_group_labeling(df, var_names, output_names)
```

## Arguments

- df:

  A data frame containing one or more columns to classify.

- var_names:

  A character vector of column names in \`df\` to which the
  classification should be applied.

- output_names:

  A character vector of prefixes for the new output columns. Must be the
  same length as \`var_names\`.

## Value

The original data frame with new columns added for each input variable:
\`\<prefix\>\_label\`, \`\<prefix\>\_num\`, \`\<prefix\>\_group_num\`,
and \`\<prefix\>\_group\`.

## Details

\- Classification values (e.g., \`"RB"\`, \`"RE_B"\`, \`"GR46"\`) are
matched to a hardcoded classification map. - The function interactively
prompts the user to resolve duplicates when a value appears in more than
one group. - An error is raised if any input values are unmatched.
