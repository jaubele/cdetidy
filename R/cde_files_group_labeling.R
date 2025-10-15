#' Apply classification mapping to CDE-style file columns
#'
#' Applies a hardcoded classification map to one or more columns in a CDE dataset. For each input
#' column, the function generates new output columns using user-defined prefixes:
#' `<prefix>_label`, `<prefix>_num`, `<prefix>_group_num`, and `<prefix>_group`.
#' Interactive prompts are used to resolve classification values that appear in multiple entries.
#'
#' @param df A data frame containing one or more columns to classify.
#' @param var_names A character vector of column names in `df` to which the classification should be applied.
#' @param output_names A character vector of prefixes for the new output columns. Must be the same length as `var_names`.
#'
#' @return The original data frame with new columns added for each input variable:
#' `<prefix>_label`, `<prefix>_num`, `<prefix>_group_num`, and `<prefix>_group`.
#'
#' @details
#' - Classification values (e.g., `"RB"`, `"RE_B"`, `"GR46"`) are matched to a hardcoded classification map.
#' - The function interactively prompts the user to resolve duplicates when a value appears in more than one group.
#' - An error is raised if any input values are unmatched.
#'
#' @export

cde_files_group_labeling <- function(df, var_names, output_names) {
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
    list(values = c("RB", "RE_B"), label = "African American", num = 1, group_num = 1, group = "Race"),
    list(values = c("RI", "RE_I"), label = "American Indian or Alaska Native", num = 2, group_num = 1, group = "Race"),
    list(values = c("RA", "RE_A"), label = "Asian", num = 3, group_num = 1, group = "Race"),
    list(values = c("RF", "RE_F"), label = "Filipino", num = 4, group_num = 1, group = "Race"),
    list(values = c("RH", "RE_H"), label = "Hispanic or Latino", num = 5, group_num = 1, group = "Race"),
    list(values = c("RP", "RE_P"), label = "Pacific Islander", num = 6, group_num = 1, group = "Race"),
    list(values = c("RT", "RE_T"), label = "Two or More Races", num = 7, group_num = 1, group = "Race"),
    list(values = c("RW", "RE_W"), label = "White", num = 8, group_num = 1, group = "Race"),
    list(values = c("RD", "RE_D"), label = "Race Not Reported", num = 9, group_num = 1, group = "Race"),
    list(values = c("GRKN", "GRK"), label = "Kindergarten", num = 10, group_num = 2, group = "Grade"),
    list(values = c("GR13"), label = "Grades 1-3", num = 11, group_num = 2, group = "Grade"),
    list(values = c("GS_46", "GR46"), label = "Grades 4-6", num = 12, group_num = 2, group = "Grade"),
    list(values = c("GR78"), label = "Grades 7-8", num = 13, group_num = 2, group = "Grade"),
    list(values = c("GRK8"), label = "Grades K-8", num = 14, group_num = 2, group = "Grade"),
    list(values = c("GS_912", "GR912"), label = "Grades 9-12", num = 15, group_num = 2, group = "Grade"),
    list(values = c("GRTKKN"), label = "Grades TK-K", num = 16, group_num = 2, group = "Grade"),
    list(values = c("GRTK8"), label = "Grades TK-8", num = 17, group_num = 2, group = "Grade"),
    list(values = c("GRTK"), label = "Grade TK", num = 18, group_num = 2, group = "Grade"),
    list(values = c("GR04"), label = "Grade 4", num = 19, group_num = 2, group = "Grade"),
    list(values = c("GR08"), label = "Grade 8", num = 20, group_num = 2, group = "Grade"),
    list(values = c("GR09"), label = "Grade 9", num = 21, group_num = 2, group = "Grade"),
    list(values = c("GR10"), label = "Grade 10", num = 22, group_num = 2, group = "Grade"),
    list(values = c("GR11"), label = "Grade 11", num = 23, group_num = 2, group = "Grade"),
    list(values = c("GR12"), label = "Grade 12", num = 24, group_num = 2, group = "Grade"),
    list(values = c("GS_PS3"), label = "Grades PreK-3", num = 25, group_num = 2, group = "Grade"),
    list(values = c("AR_03"), label = "Children K-12 who are 0-3", num = 26, group_num = 3, group = "Age Range"),
    list(values = c("AR_0418"), label = "Children K-12 who are 4-18", num = 27, group_num = 3, group = "Age Range"),
    list(values = c("AR_1922"), label = "Continuing Students K-12 who are 19-12", num = 28, group_num = 3, group = "Age Range"),
    list(values = c("AR_2329"), label = "Adults K-12 who are 23-29", num = 29, group_num = 3, group = "Age Range"),
    list(values = c("AR_3039"), label = "Adults K-12 who are 30-39", num = 30, group_num = 3, group = "Age Range"),
    list(values = c("AR_4049"), label = "Adults K-12 who are 40-49", num = 31, group_num = 3, group = "Age Range"),
    list(values = c("AR_50P"), label = "Adults K-12 who are 50 plus", num = 32, group_num = 3, group = "Age Range"),
    list(values = c("GN_F", "GF"), label = "Female", num = 33, group_num = 4, group = "Gender"),
    list(values = c("GN_M", "GM"), label = "Male", num = 34, group_num = 4, group = "Gender"),
    list(values = c("GN_X", "GX", "GN"), label = "Non-Binary", num = 35, group_num = 4, group = "Gender"),
    list(values = c("GN_Z", "GX", "GZ"), label = "Gender Missing", num = 36, group_num = 4, group = "Gender"),
    list(values = c("SG_EL", "SE", "ELAS_EL"), label = "English Learner", num = 37, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("SG_DS", "SD"), label = "Students with Disabilities", num = 38, group_num = 5, group = "Student Subgroup"),
    list(values = c("SG_SD", "SS"), label = "Socioeconomically Disadvantaged", num = 39, group_num = 5, group = "Student Subgroup"),
    list(values = c("SG_MG", "SM"), label = "Migrant Youth", num = 40, group_num = 5, group = "Student Subgroup"),
    list(values = c("SG_FS", "SF"), label = "Foster Youth", num = 41, group_num = 5, group = "Student Subgroup"),
    list(values = c("SG_HM", "SH"), label = "Homeless Youth", num = 42, group_num = 5, group = "Student Subgroup"),
    list(values = c("S5"), label = "Student with a 504 Accommodation Plan", num = 43, group_num = 5, group = "Student Subgroup"),
    list(values = c("HUYN"), label = "Not Homeless Unaccompanied Youth", num = 44, group_num = 5, group = "Student Subgroup"),
    list(values = c("HUYY"), label = "Homeless Unaccompanied Youth", num = 45, group_num = 5, group = "Student Subgroup"),
    list(values = c("EL_Y"), label = "Is an English Learner", num = 46, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("EL_Y"), label = "Is Not an English Learner", num = 47, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("CAY"), label = "Is Chronically Absent", num = 48, group_num = 5, group = "Student Subgroup"),
    list(values = c("CAN"), label = "Is Not Chronically Absent", num = 49, group_num = 5, group = "Student Subgroup"),
    list(values = c("TA"), label = "Total Number of Students", num = 50, group_num = 6, group = "Total Number of Students"),
    list(values = c("ELAS_ADEL"), label = "Adult English Learner", num = 51, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("ELAS_EO"), label = "English Only", num = 52, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("ELAS_IFEP"), label = "Initial Fluent English Proficient", num = 53, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("ELAS_MISS"), label = "EL Status Missing", num = 54, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("ELAS_RFEP"), label = "Reclassified Fluent English Proficient", num = 55, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("ELAS_TBD"), label = "English Status TBD", num = 56, group_num = 8, group = "English Language Acquisition Status"),
    list(values = c("DC_AUT"), label = "Autism", num = 57, group_num = 7, group = "Disability Category"),
    list(values = c("DC_DB"), label = "Deaf Blindedness", num = 58, group_num = 7, group = "Disability Category"),
    list(values = c("DC_DFHI"), label = "Deaf/Hearing Impairment", num = 59, group_num = 7, group = "Disability Category"),
    list(values = c("DC_ED"), label = "Emotional Disturbance", num = 60, group_num = 7, group = "Disability Category"),
    list(values = c("DC_EMD"), label = "Established Medical Disability", num = 61, group_num = 7, group = "Disability Category"),
    list(values = c("DC_HH"), label = "Hard of Hearing", num = 62, group_num = 7, group = "Disability Category"),
    list(values = c("DC_ID"), label = "Intellectual Disability", num = 63, group_num = 7, group = "Disability Category"),
    list(values = c("DC_MD"), label = "Multiple Disabilities", num = 64, group_num = 7, group = "Disability Category"),
    list(values = c("DC_OHI"), label = "Other Health Impairment", num = 65, group_num = 7, group = "Disability Category"),
    list(values = c("DC_OI"), label = "Orthopedic Health Impairment", num = 66, group_num = 7, group = "Disability Category"),
    list(values = c("DC_SLD"), label = "Specific Learning Disability", num = 67, group_num = 7, group = "Disability Category"),
    list(values = c("DC_SLI"), label = "Speech or Language Impairment", num = 68, group_num = 7, group = "Disability Category"),
    list(values = c("DC_TBI"), label = "Traumatic Brain Injury", num = 69, group_num = 7, group = "Disability Category"),
    list(values = c("DC_VI"), label = "Visual Impairment", num = 70, group_num = 7, group = "Disability Category"),
    list(values = c("AR_35"), label = "Ages 3-5", num = 71, group_num = 3, group = "Age Range"),
    list(values = c("AR_612"), label = "Ages 6-12", num = 72, group_num = 3, group = "Age Range"),
    list(values = c("AR_1318"), label = "Ages 13-18", num = 73, group_num = 3, group = "Age Range"),
    list(values = c("AR_19P"), label = "Ages 19 plus", num = 74, group_num = 3, group = "Age Range"),
    list(values = c("ALL"), label = "All data for all schools", num = 75, group_num = 9, group = "Grade Span"),
    list(values = c("GS_K6", "GRK6"), label = "School grade span K-6", num = 76, group_num = 9, group = "Grade Span"),
    list(values = c("GS_69", "GR69"), label = "School grade span 6-9", num = 77, group_num = 9, group = "Grade Span"),
    list(values = c("GS_912", "GR912"), label = "School grade span 9-12", num = 78, group_num = 9, group = "Grade Span"),
    list(values = c("GS_K12", "GRK12"), label = "School grade span K-12", num = 79, group_num = 9, group = "Grade Span")
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
