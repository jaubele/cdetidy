#' Apply classification mapping across multiple assessment file columns
#'
#' This function applies a hardcoded classification map to one or more columns in an assessment dataset.
#' It dynamically adds new variables for label, numeric code, group number, and group name using
#' user-defined output prefixes. It also includes interactive prompts to resolve duplicate values that
#' appear across multiple classifications. Grade values are temporarily recoded to match the map,
#' then restored after mapping is complete.
#'
#' @param df A data frame containing one or more columns to classify.
#' @param var_names A character vector of column names in `df` to which the classification should be applied.
#' @param output_names A character vector of prefixes for new output columns. Must be the same length as `var_names`.
#'
#' @return The original data frame with new columns added for each input variable:
#' `<prefix>_label`, `<prefix>_num`, `<prefix>_group_num`, and `<prefix>_group`.
#'
#' @details
#' - Values in the "grade" column are temporarily recoded to match expected numeric keys in the classification map.
#' - The function will stop and report any unmatched values.
#' - Duplicate values across multiple classification entries will prompt the user to choose which label to assign.
#'
#' @export

assessment_files_group_labeling <- function(df, var_names, output_names) {
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

  # ---- INTERNAL: Hardcoded column-specific value mutation ----
  if ("grade" %in% names(df)) {
    
    df$grade_original <- df$grade
    
    # Recode directly on the character vector
    df$grade[df$grade == "KN"] <- "-1"   # Kindergarten sentinel
    # KN needs to become numeric, as it's the only character value in list
    
    grade_num <- as.numeric(df$grade)
    
    grade_num <- dplyr::recode(
      grade_num,
      `99` = 991,
      `8` = 81,
      .default = grade_num)
    
    df$grade <- grade_num
  }

  # Sample hardcoded classification map
  classification_map <- list(
    list(values = c(74), label = "Black or African American", num = 1, group_num = 1, group = "Race"),
    list(values = c(75), label = "American Indian or Alaska Native", num = 2, group_num = 1, group = "Race"),
    list(values = c(76), label = "Asian", num = 3, group_num = 1, group = "Race"),
    list(values = c(77), label = "Filipino", num = 4, group_num = 1, group = "Race"),
    list(values = c(78), label = "Hispanic or Latino", num = 5, group_num = 1, group = "Race"),
    list(values = c(79), label = "Native Hawaiian or Pacific Islander", num = 6, group_num = 1, group = "Race"),
    list(values = c(144), label = "Two or More Races", num = 7, group_num = 1, group = "Race"),
    list(values = c(80), label = "White", num = 8, group_num = 1, group = "Race"),
    list(values = c(-1), label = "Kindergarten", num = 9, group_num = 2, group = "Grade"),
    list(values = c(1, "01"), label = "Grade 1", num = 10, group_num = 2, group = "Grade"),
    list(values = c(2, "02"), label = "Grade 2", num = 11, group_num = 2, group = "Grade"),
    list(values = c(3, "03"), label = "Grade 3", num = 12, group_num = 2, group = "Grade"),
    list(values = c(4, "04"), label = "Grade 4", num = 13, group_num = 2, group = "Grade"),
    list(values = c(5, "05"), label = "Grade 5", num = 14, group_num = 2, group = "Grade"),
    list(values = c(6, "06"), label = "Grade 6", num = 15, group_num = 2, group = "Grade"),
    list(values = c(7, "07"), label = "Grade 7", num = 16, group_num = 2, group = "Grade"),
    list(values = c(81, "08"), label = "Grade 8", num = 17, group_num = 2, group = "Grade"), #manual c() value to avoid forced duplicates
    list(values = c(9, "09"), label = "Grade 9", num = 18, group_num = 2, group = "Grade"),
    list(values = c(10, "10"), label = "Grade 10", num = 19, group_num = 2, group = "Grade"),
    list(values = c(11, "11"), label = "Grade 11", num = 20, group_num = 2, group = "Grade"),
    list(values = c(12, "12"), label = "Grade 12", num = 21, group_num = 2, group = "Grade"),
    list(values = c(13, "13"), label = "All Grades", num = 22, group_num = 2, group = "Grade"),
    list(values = c(14), label = "All High School Grades", num = 23, group_num = 2, group = "Grade"),
    list(values = c(991), label = "Cohort Grade/Graduating Class", num = 24, group_num = 2, group = "Grade"), #manual c() value to avoid forced duplicates
    list(values = c(4), label = "Female", num = 25, group_num = 3, group = "Gender"),
    list(values = c(3), label = "Male", num = 26, group_num = 3, group = "Gender"),
    list(values = c(28), label = "Migrant Youth", num = 27, group_num = 4, group = "Student Subgroup"),
    list(values = c(29), label = "Not Migrant Youth", num = 28, group_num = 4, group = "Student Subgroup"),
    list(values = c(52), label = "Homeless Youth", num = 29, group_num = 4, group = "Student Subgroup"),
    list(values = c(53), label = "Not Homeless Youth", num = 30, group_num = 4, group = "Student Subgroup"),
    list(values = c(240), label = "Foster Youth", num = 31, group_num = 4, group = "Student Subgroup"),
    list(values = c(241), label = "Not Foster Youth", num = 32, group_num = 4, group = "Student Subgroup"),
    list(values = c(50), label = "Armed Forces Family Member", num = 33, group_num = 4, group = "Student Subgroup"),
    list(values = c(51), label = "No Armed Forces Family Member", num = 34, group_num = 4, group = "Student Subgroup"),
    list(values = c(1), label = "All Students", num = 35, group_num = 4, group = "Student Subgroup"),
    list(values = c(128), label = "Student with a Disability", num = 36, group_num = 4, group = "Student Subgroup"),
    list(values = c(99), label = "Student Without a Disability", num = 37, group_num = 4, group = "Student Subgroup"),
    list(values = c(239), label = "Student with a Disability Tested with Alternate Assessment", num = 38, group_num = 5, group = "Test Taken"),
    list(values = c(31), label = "Socioeconomically Disadvantaged", num = 39, group_num = 4, group = "Student Subgroup"),
    list(values = c(111), label = "Not Socioeconomically Disadvantaged", num = 40, group_num = 4, group = "Student Subgroup"),
    list(values = c(6), label = "IFEP, RFEP and EO", num = 41, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(7), label = "IFEP", num = 42, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(8), label = "RFEP", num = 43, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(120), label = "ELs Enrolled Less Than 12 Months", num = 44, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(142), label = "ELs Enrolled 12 Months or More", num = 45, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(160), label = "English Learner", num = 46, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(243), label = "Adult English Learner Only", num = 47, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(180), label = "English Only", num = 48, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(170), label = "Ever-EL", num = 49, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(250), label = "Long-Term English Learner", num = 50, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(251), label = "At-Risk of Becoming LTEL", num = 51, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(252), label = "Never an English Learner", num = 52, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(190), label = "English Language Acquisition Status TBD", num = 53, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(242), label = "English Learner - 1 Year in Program", num = 54, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(243), label = "English Learner - 2 Years in Program", num = 55, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(244), label = "English Learner - 3 Years in Program", num = 56, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(245), label = "English Learner - 4 Years in Program", num = 57, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(246), label = "English Learner - 5 Years in Program", num = 58, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(247), label = "English Learner - 6 Years in Program", num = 59, group_num = 6, group = "English Language Acquisition Status"),
    list(values = c(228), label = "Spanish", num = 60, group_num = 7, group = "First Language"),
    list(values = c(229), label = "Vietnamese", num = 61, group_num = 7, group = "First Language"),
    list(values = c(230), label = "Mandarin (Putonghua)", num = 62, group_num = 7, group = "First Language"),
    list(values = c(231), label = "Arabic", num = 63, group_num = 7, group = "First Language"),
    list(values = c(232), label = "Filipino (Pilipino or Tagalog)", num = 64, group_num = 7, group = "First Language"),
    list(values = c(233), label = "Cantonese", num = 65, group_num = 7, group = "First Language"),
    list(values = c(234), label = "Korean", num = 66, group_num = 7, group = "First Language"),
    list(values = c(235), label = "Hmong", num = 67,group_num = 7,  group = "First Language"),
    list(values = c(236), label = "Punjabi", num = 68, group_num = 7, group = "First Language"),
    list(values = c(237), label = "Russian", num = 69, group_num = 7, group = "First Language"),
    list(values = c(238), label = "All Remaining Languages", num = 70, group_num = 7, group = "First Language"),
    list(values = c(90), label = "Not a High School Graduate", num = 71, group_num = 8, group = "Parent Education"),
    list(values = c(91), label = "High School Graduate", num = 72, group_num = 8, group = "Parent Education"),
    list(values = c(92), label = "Some College (Includes AA Degree)", num = 73, group_num = 8, group = "Parent Education"),
    list(values = c(93), label = "College Graduate", num = 74, group_num = 8, group = "Parent Education"),
    list(values = c(94), label = "Graduate School/Post Graduate", num = 75, group_num = 8, group = "Parent Education"),
    list(values = c(121), label = "Declined to State", num = 76, group_num = 8, group = "Parent Education"),
    list(values = c(201), label = "American Indian or Alaska Native and Economically Disadvantaged", num = 77, group_num = 9, group = "Crosstabs"),
    list(values = c(202), label = "Asian and Economically Disadvantaged", num = 78, group_num = 9, group = "Crosstabs"),
    list(values = c(200), label = "Black or African American and Economically Disadvantaged", num = 79, group_num = 9, group = "Crosstabs"),
    list(values = c(203), label = "Filipino and Economically Disadvantaged", num = 80, group_num = 9, group = "Crosstabs"),
    list(values = c(204), label = "Hispanic or Latino and Economically Disadvantaged", num = 81, group_num = 9, group = "Crosstabs"),
    list(values = c(205), label = "Native Hawaiian or Pacific Islander and Economically Disadvantaged", num = 82, group_num = 9, group = "Crosstabs"),
    list(values = c(206), label = "White and Economically Disadvantaged", num = 83, group_num = 9, group = "Crosstabs"),
    list(values = c(207), label = "Two or More Races and Economically Disadvantaged", num = 84, group_num = 9, group = "Crosstabs"),
    list(values = c(221), label = "American Indian or Alaska Native and Not Economically Disadvantaged", num = 85, group_num = 9, group = "Crosstabs"),
    list(values = c(222), label = "Asian and Not Economically Disadvantaged", num = 86, group_num = 9, group = "Crosstabs"),
    list(values = c(220), label = "Black or African American and Not Economically Disadvantaged", num = 87, group_num = 9, group = "Crosstabs"),
    list(values = c(223), label = "Filipino and Not Economically Disadvantaged", num = 88, group_num = 9, group = "Crosstabs"),
    list(values = c(224), label = "Hispanic or Latino and Not Economically Disadvantaged", num = 89, group_num = 9, group = "Crosstabs"),
    list(values = c(225), label = "Native Hawaiian or Pacific Islander and Not Economically Disadvantaged", num = 90, group_num = 9, group = "Crosstabs"),
    list(values = c(226), label = "White and Not Economically Disadvantaged", num = 91, group_num = 9, group = "Crosstabs"),
    list(values = c(227), label = "Two or More Races and Not Economically Disadvantaged", num = 92, group_num = 9, group = "Crosstabs"),
    list(values = c(248), label = "English Learner - Less than 1 Year in Program", num = 93, group_num = 6, group = "English Language Acquisition Status")
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

  if ("grade_original" %in% names(df)) {
    df$grade <- df$grade_original
    df$grade_original <- NULL  # optional: clean up
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
