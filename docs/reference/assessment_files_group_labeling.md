# Apply classification mapping across multiple assessment file columns

This function applies a hardcoded classification map to one or more
columns in an assessment dataset. It dynamically adds new variables for
label, numeric code, group number, and group name using user-defined
output prefixes. It also includes interactive prompts to resolve
duplicate values that appear across multiple classifications. Grade
values are temporarily recoded to match the map, then restored after
mapping is complete.

## Usage

``` r
assessment_files_group_labeling(df, var_names, output_names)
```

## Arguments

- df:

  A data frame containing one or more columns to classify.

- var_names:

  A character vector of column names in \`df\` to which the
  classification should be applied.

- output_names:

  A character vector of prefixes for new output columns. Must be the
  same length as \`var_names\`.

## Value

The original data frame with new columns added for each input variable:
\`\<prefix\>\_label\`, \`\<prefix\>\_num\`, \`\<prefix\>\_group_num\`,
and \`\<prefix\>\_group\`.

## Details

\- Values in the "grade" column are temporarily recoded to match
expected numeric keys in the classification map. - The function will
stop and report any unmatched values. - Duplicate values across multiple
classification entries will prompt the user to choose which label to
assign.
