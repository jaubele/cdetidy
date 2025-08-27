#' Apply dashboard classification mapping to one or more columns
#'
#' Applies a hardcoded classification map to one or more columns in a dashboard-oriented dataset.
#' For each input column, the function generates new output columns using user-defined prefixes:
#' `<prefix>_label`, `<prefix>_num`, `<prefix>_group_num`, and `<prefix>_group`.
#' It also interactively resolves duplicate codes that appear in more than one group.
#'
#' @param df A data frame containing one or more columns to classify.
#' @param var_names A character vector of column names in `df` to which the classification should be applied.
#' @param output_names A character vector of prefixes for the new output columns. Must be the same length as `var_names`.
#'
#' @return The original data frame with new columns added for each input variable:
#' `<prefix>_label`, `<prefix>_num`, `<prefix>_group_num`, and `<prefix>_group`.
#'
#' @details
#' - Classification values like `"AA"`, `"EL"`, `"SED"` are matched to a built-in classification map.
#' - Duplicate values across multiple entries prompt the user to choose which label to apply.
#' - The function raises an error if any input values are unmatched.
#'
#' @export

dashboard_files_group_labeling <- function(df, var_names, output_names) {
  if (length(var_names) != length(output_names)) {
    stop("You must have the same number of variables as outputs")
  }

  missing_cols <- setdiff(var_names, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in dataframe:", paste(missing_cols, collapse = ", ")))
  }

  # ---- Internal helper: Only prompt if duplicates are also in the data ----
  resolve_duplicates_interactively <- function(classification_map, df, var_names) {
    all_values <- unlist(lapply(classification_map, function(x) x$values))
    dupes <- unique(all_values[duplicated(all_values)])

    # Get all values in the data across the relevant columns
    data_values <- unique(unlist(as.data.frame(df)[, var_names]))

    # Only keep duplicated values that also appear in the data
    relevant_dupes <- intersect(dupes, data_values)
    if (length(relevant_dupes) == 0) return(classification_map)

    message("Duplicate classification values found in your data. Please choose which label to apply:")

    resolved_map <- list()

    for (dup in relevant_dupes) {
      matching_entries <- Filter(function(x) dup %in% x$values, classification_map)

      cat("\nValue:", dup, "\n")
      for (i in seq_along(matching_entries)) {
        cat(i, "-> label:", matching_entries[[i]]$label,
            "| num:", matching_entries[[i]]$num,
            "| group_num:", matching_entries[[i]]$group_num,
            "| group:", matching_entries[[i]]$group, "\n")
      }

      choice <- as.integer(readline(prompt = "Select the number of the preferred option: "))
      selected <- matching_entries[[choice]]
      selected$values <- dup  # Keep only selected value

      # Remove the value from all other entries in the full map
      classification_map <- lapply(classification_map, function(x) {
        x$values <- setdiff(x$values, dup)
        x
      })

      # Add selected entry back
      resolved_map <- append(resolved_map, list(selected))
    }

    # Keep all other non-duplicate or unresolved entries
    unique_map <- Filter(function(x) all(!x$values %in% relevant_dupes), classification_map)
    final_map <- append(unique_map, resolved_map)

    return(final_map)
  }

  # Sample hardcoded classification map
  classification_map <- list(
    list(values = c("AA"), label = "Black/African American", num = 1, group_num = 1, group = "Race"),
    list(values = c("AI"), label = "American Indian or Alaska Native", num = 2, group_num = 1, group = "Race"),
    list(values = c("AS"), label = "Asian", num = 3, group_num = 1, group = "Race"),
    list(values = c("FI"), label = "Filipino", num = 4, group_num = 1, group = "Race"),
    list(values = c("HI"), label = "Hispanic", num = 5, group_num = 1, group = "Race"),
    list(values = c("PI"), label = "Pacific Islander", num = 6, group_num = 1, group = "Race"),
    list(values = c("MR"), label = "Multiple Races/Two or More", num = 7, group_num = 1, group = "Race"),
    list(values = c("WH"), label = "White", num = 8, group_num = 1, group = "Race"),
    list(values = c("SED"), label = "Socioeconomically Disadvantaged", num = 9, group_num = 2, group = "Student Subgroup"),
    list(values = c("SWD"), label = "Students with Disabilities", num = 10, group_num = 2, group = "Student Subgroup"),
    list(values = c("FOS"), label = "Foster Youth", num = 11, group_num = 2, group = "Student Subgroup"),
    list(values = c("HOM"), label = "Homeless Youth", num = 12, group_num = 2, group = "Student Subgroup"),
    list(values = c("RFP"), label = "Recently Reclassified Fluent-English Proficient Only", num = 13, group_num = 4, group = "English Language Acquisition Status"),
    list(values = c("LTEL"), label = "Long-Term English Learner", num = 14, group_num = 2, group = "Student Subgroup"),
    list(values = c("SBA"), label = "Students Who Took SBAC", num = 15, group_num = 3, group = "Test Taken"),
    list(values = c("CAA"), label = "Students Who Took CAA", num = 16, group_num = 3, group = "Test Taken"),
    list(values = c("CAST"), label = "Students Who Took CAST", num = 17, group_num = 3, group = "Test Taken"),
    list(values = c("EL"), label = "English Learner", num = 18, group_num = 4, group = "English Language Acquisition Status"),
    list(values = c("ELO"), label = "English Learners Only", num = 19, group_num = 4, group = "English Language Acquisition Status"),
    list(values = c("EO"), label = "English Only", num = 20, group_num = 4, group = "English Language Acquisition Status"),
    list(values = c("ALL"), label = "All Students", num = 21, group_num = 5, group = "All Students")
  )

  # ---- Resolve only relevant duplicates ----
  classification_map <- resolve_duplicates_interactively(classification_map, df, var_names)

  # ---- Apply classification across each column ----
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    prefix <- output_names[i]

    label_col <- paste0(prefix, "_label")
    num_col  <- paste0(prefix, "_num")
    group_num_col <- paste0(prefix, "_group_num")
    group_col <- paste0(prefix, "_group")

    df[[label_col]] <- NA_character_
    df[[num_col]]  <- NA_real_
    df[[group_num_col]] <- NA_real_
    df[[group_col]] <- NA_character_

    current_values <- df[[var]]

    for (entry in classification_map) {
      matched <- ifelse(current_values %in% entry$values, TRUE, FALSE)

      if (any(matched, na.rm = TRUE)) {
        df[[label_col]] <- ifelse(is.na(df[[label_col]]) & matched, entry$label, df[[label_col]])
        df[[num_col]]  <- ifelse(is.na(df[[num_col]])  & matched, entry$num,  df[[num_col]])
        df[[group_num_col]] <- ifelse(is.na(df[[group_num_col]]) & matched, entry$group_num, df[[group_num_col]])
        df[[group_col]] <- ifelse(is.na(df[[group_col]]) & matched, entry$group, df[[group_col]])
      }
    }
  }

  # ---- Final validation: Stop if unmapped values are found ----
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    prefix <- output_names[i]
    label_col <- paste0(prefix, "_label")

    # Only consider rows where input is not NA but label is NA
    unmatched_vals <- unique(df[[var]][is.na(df[[label_col]]) & !is.na(df[[var]])])

    if (length(unmatched_vals) > 0) {
      stop(
        "❌ The following ", length(unmatched_vals), " value(s) in column '", var,
        "' could not be mapped:\n→ ",
        paste(unmatched_vals, collapse = ", "),
        "\nPlease update the classification map to include these values."
      )
    }
  }

  return(df)
}
