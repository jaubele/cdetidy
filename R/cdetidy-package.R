#' cdetidy: Tools for Working with CDE Datasets
#'
#' Utilities for classification, labeling, schema validation, and export of
#' public California Department of Education datasets.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map_int map_lgl
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom data.table := fread fwrite
#' @importFrom stats na.omit
#' @importFrom utils head read.csv write.csv write.table
#' @keywords internal
"_PACKAGE"
