#' Convert a classification entry into a dimension table row
#'
#' Transforms a single list entry from a classification map into a standardized row
#' for inclusion in a dimension table. The output includes the group label, numeric ID,
#' group number, and group name.
#'
#' @param entry A list containing a classification entry with the elements:
#' - `label`: A character label (e.g., "Hispanic or Latino")
#' - `num`: A numeric code for the label
#' - `group_num`: A numeric ID for the group category
#' - `group`: A character name for the group category
#'
#' @return A one-row tibble with columns: `demo_group`, `demo_group_num`, `demo_group_group_num`, and `demo_group_group`.
#'
#' @export

class_map_to_dim <- function(entry) {
  tibble::tibble(
    demo_group = entry$label,
    demo_group_num = entry$num,
    demo_group_group_num = entry$group_num,
    demo_group_group = entry$group
  )
}
