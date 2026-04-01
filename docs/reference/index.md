# Package index

## Classification & Group Labeling

Functions for applying classification maps and labeling group variables.

- [`assessment_files_group_labeling()`](https://jaubele.github.io/cdetidy/reference/assessment_files_group_labeling.md)
  : Apply classification mapping across multiple assessment file columns
- [`cde_files_group_labeling()`](https://jaubele.github.io/cdetidy/reference/cde_files_group_labeling.md)
  : Apply classification mapping to CDE-style file columns
- [`dashboard_files_group_labeling()`](https://jaubele.github.io/cdetidy/reference/dashboard_files_group_labeling.md)
  : Apply dashboard classification mapping to one or more columns
- [`class_map_to_dim()`](https://jaubele.github.io/cdetidy/reference/class_map_to_dim.md)
  : Convert a classification entry into a dimension table row
- [`validate_group_mapping_tabyl()`](https://jaubele.github.io/cdetidy/reference/validate_group_mapping_tabyl.md)
  : Validate that an old-to-new group mapping is one-to-one

## CDS Code Utilities

Tools for padding, expanding, and resolving CDS codes.

- [`pad_cds_codes()`](https://jaubele.github.io/cdetidy/reference/pad_cds_codes.md)
  : Pad county, district, and school codes and optionally generate CDS
- [`expand_cds_codes()`](https://jaubele.github.io/cdetidy/reference/expand_cds_codes.md)
  : Expand a full CDS code into county, district, and school components
- [`verify_padded_cds()`](https://jaubele.github.io/cdetidy/reference/verify_padded_cds.md)
  : Verify that CDS and component columns are correctly padded
- [`get_conflicting_cds()`](https://jaubele.github.io/cdetidy/reference/get_conflicting_cds.md)
  : Identify CDS codes linked to multiple organization levels
- [`replace_conflicting_cds()`](https://jaubele.github.io/cdetidy/reference/replace_conflicting_cds.md)
  : Resolve conflicting CDS codes by appending a suffix to school-level
  entries
- [`resolve_conflicting_cds()`](https://jaubele.github.io/cdetidy/reference/resolve_conflicting_cds.md)
  : Identify and resolve conflicting CDS codes in a dataset

## Schema & SQL Generation

Tools for generating and validating SQL table structures and
constraints.

- [`sql_schema_log()`](https://jaubele.github.io/cdetidy/reference/sql_schema_log.md)
  : Generate and log a SQL schema for a data frame
- [`flag_schema_changes()`](https://jaubele.github.io/cdetidy/reference/flag_schema_changes.md)
  : Flag schema changes by comparing to a prior-year archive
- [`sql_table_coder()`](https://jaubele.github.io/cdetidy/reference/sql_table_coder.md)
  : Generate SQL CREATE TABLE statements from schema metadata
- [`sql_insert_coder()`](https://jaubele.github.io/cdetidy/reference/sql_insert_coder.md)
  : Generate SQL BULK INSERT statements for warehouse flat files
- [`sql_foreign_key_coder()`](https://jaubele.github.io/cdetidy/reference/sql_foreign_key_coder.md)
  : Generate SQL scripts to add foreign key constraints
- [`sql_ident()`](https://jaubele.github.io/cdetidy/reference/sql_ident.md)
  : Safely quote SQL Server identifiers
- [`fk_constraint_names()`](https://jaubele.github.io/cdetidy/reference/fk_constraint_names.md)
  : Generate deterministic foreign key constraint names from schema
  metadata
- [`fk_dim()`](https://jaubele.github.io/cdetidy/reference/fk_dim.md) :
  Generate foreign key name for a dimension table

## Export & Logging

Functions for safely writing and logging fact/dimension tables.

- [`safe_fwrite()`](https://jaubele.github.io/cdetidy/reference/safe_fwrite.md)
  : Safely write a dataset to CSV with metadata logging
- [`safe_fwrite_warehouse()`](https://jaubele.github.io/cdetidy/reference/safe_fwrite_warehouse.md)
  : Safely write a warehouse-ready CSV file with metadata logging
- [`export_with_schema()`](https://jaubele.github.io/cdetidy/reference/export_with_schema.md)
  : Export a dataset with schema logging
- [`export_with_schema_warehouse()`](https://jaubele.github.io/cdetidy/reference/export_with_schema_warehouse.md)
  : Export a dataset to the warehouse with schema logging
- [`sql_schema_log_warehouse()`](https://jaubele.github.io/cdetidy/reference/sql_schema_log_warehouse.md)
  : Generate and log a SQL schema for a data frame
- [`filter_schema_sources()`](https://jaubele.github.io/cdetidy/reference/filter_schema_sources.md)
  : Filter a schema log for a specific data source (with optional
  universal tables)
- [`normalize_for_compare()`](https://jaubele.github.io/cdetidy/reference/normalize_for_compare.md)
  : Normalize schema log values for comparison

## Validation Utilities

Functions to validate keys, suppression, and SQL-safe names.

- [`validate_primary_key()`](https://jaubele.github.io/cdetidy/reference/validate_primary_key.md)
  : Validate primary key uniqueness in a dataset
- [`validate_sql_identifiers()`](https://jaubele.github.io/cdetidy/reference/validate_sql_identifiers.md)
  : Validate SQL-safe table or column names
- [`check_suppression_dependency()`](https://jaubele.github.io/cdetidy/reference/check_suppression_dependency.md)
  : Check suppression dependency across numeric columns
- [`compare_variable_names()`](https://jaubele.github.io/cdetidy/reference/compare_variable_names.md)
  : Compare column names between two data frames
- [`validate_combined_data()`](https://jaubele.github.io/cdetidy/reference/validate_combined_data.md)
  : Validate a joined vs. distinct dataset and return the deduplicated
  frame
- [`coerce_suppression_cols()`](https://jaubele.github.io/cdetidy/reference/coerce_suppression_cols.md)
  : Coerce columns with suppression characters into numeric or integer
- [`star_scan()`](https://jaubele.github.io/cdetidy/reference/star_scan.md)
  : Scan a data frame for pattern-matching values (e.g., "\*")
- [`zero_inflation_diff()`](https://jaubele.github.io/cdetidy/reference/zero_inflation_diff.md)
  : Compare zero-value rates across two datasets
- [`compare_suppression_columns()`](https://jaubele.github.io/cdetidy/reference/compare_suppression_columns.md)
  : Compare suppression-affected columns across two datasets

## Dimension Management

Helpers for identifying, cleaning, or clearing dimension-like objects.

- [`get_dim_objects()`](https://jaubele.github.io/cdetidy/reference/get_dim_objects.md)
  : List dimension-like objects in the global environment
- [`clear_dim_objects()`](https://jaubele.github.io/cdetidy/reference/clear_dim_objects.md)
  : Remove dimension-related objects from the global environment

## File & Test Utilities

Utilities for file selection, comparison, and test file splitting.

- [`files_by_keywords()`](https://jaubele.github.io/cdetidy/reference/files_by_keywords.md)
  : Search for and select files by keyword from a folder
- [`load_and_compare_files()`](https://jaubele.github.io/cdetidy/reference/load_and_compare_files.md)
  : Load, standardize ID codes, compare schemas, and optionally join
  files
- [`split_sbac_test()`](https://jaubele.github.io/cdetidy/reference/split_sbac_test.md)
  : Split SBAC test data into ELA and Math datasets

## Script Orchestration

High-level orchestration for warehouse build and SQL automation.

- [`generate_sql_script()`](https://jaubele.github.io/cdetidy/reference/generate_sql_script.md)
  : Generate a T-SQL script for loading warehouse tables
- [`archive_schema_log()`](https://jaubele.github.io/cdetidy/reference/archive_schema_log.md)
  : Archive the primary SQL schema log for a given year
