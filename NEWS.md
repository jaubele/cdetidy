# cdetidy 0.1.5 cleanup

This focused cleanup strengthens the shared helpers used by the streamlined
CDE pipelines without redesigning file-specific transformations.

## Changes

- `validate_group_mapping_tabyl()` now validates a many-to-one mapping. Every
  source code must map to one non-missing standardized value; several source
  codes may share the same standardized value.
- `compare_variable_names()` and `compare_suppression_columns()` still print
  interactive QA messages and now invisibly return structured tibbles that can
  be retained in pipeline results.
- CDS conflict resolution preserves both cross-level and same-level conflict
  metadata and consistently respects a custom altered-row flag column.
- Documentation now consistently defines `altered_cds` as a 0/1 flag.
- Duplicate reporting and result-construction logic was removed from
  `validate_primary_key()`.
- Runtime dependencies are declared, core helper calls are namespaced, and a
  testthat suite covers the changed behavior.

## Deferred mapping redesign

Genuine context-dependent classification conflicts remain interactive. They
should be addressed by separating assessment mappings by source-column role
(for example, grade versus demographic group) when the assessment pipelines
are reviewed.

## Validate in R

Run from the package project:

```r
devtools::document()
devtools::test()
devtools::check()
```

Run `devtools::document()` before committing so `NAMESPACE` and affected files
under `man/` are regenerated from the updated roxygen comments.
