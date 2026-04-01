# Generate foreign key name for a dimension table

Appends the suffix \`"\_dim"\` to a given base name, typically to
represent a foreign key column that links to a dimension table.

## Usage

``` r
fk_dim(name)
```

## Arguments

- name:

  A character string representing the base name of the dimension (e.g.,
  \`"student"\`, \`"school"\`).

## Value

A character string with \`"\_dim"\` appended (e.g., \`"student_dim"\`).
