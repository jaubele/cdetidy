#' Search for and select files by keyword from a folder
#'
#' Searches a specified folder for filenames that match any of the given keywords,
#' then presents an interactive menu for the user to select files to return.
#'
#' @param folder A string path to the folder where files will be searched.
#' @param keywords A character vector of one or more keywords to match against filenames.
#' @param recursive Logical. If `TRUE`, search subdirectories as well. Default is `TRUE`.
#' @param ignore_case Logical. If `TRUE`, matching is case-insensitive. Default is `TRUE`.
#' @param full_names Logical. If `TRUE`, returns full paths; otherwise, returns filenames only. Default is `TRUE`.
#'
#' @return A character vector of selected file paths. If the user cancels or no files match,
#' an empty character vector is returned.
#'
#' @details
#' - The function filters files based on whether any keyword appears in the filename (not the full path).
#' - If one or more files match, the user is prompted to select files by number.
#' - If no keywords match or the user cancels, nothing is returned.
#' @export

files_by_keywords <- function(
    folder,
    keywords,
    recursive = TRUE,
    ignore_case = TRUE,
    full_names = TRUE
) {
  # Check folder exists
  if (!dir.exists(folder)) {
    stop("❌ The specified folder does not exist: ", folder)
  }

  if (length(keywords) == 0) {
    stop("❌ Please supply at least one keyword to search for.")
  }

  # List files
  files <- list.files(path = folder, recursive = recursive, full.names = full_names)

  # Filter based on keyword pattern
  pattern <- paste0(keywords, collapse = "|")
  matched_files <- files[grepl(pattern, basename(files), ignore.case = ignore_case)]

  if (length(matched_files) == 0) {
    message("🔍 No files matched the keywords: ", paste(keywords, collapse = ", "))
    return(character(0))
  }

  # Show menu for selection
  cat("📂 Matched files:\n")
  choices <- basename(matched_files)
  for (i in seq_along(choices)) {
    cat(sprintf("[%2d] %s\n", i, choices[i]))
  }

  cat("\nEnter numbers of files to select (comma-separated), or press Enter to cancel: ")
  input <- readline()

  if (input == "") {
    message("🚫 Selection cancelled.")
    return(character(0))
  }

  selected_indices <- suppressWarnings(as.integer(strsplit(input, ",")[[1]]))
  selected_indices <- selected_indices[!is.na(selected_indices) & selected_indices >= 1 & selected_indices <= length(matched_files)]

  if (length(selected_indices) == 0) {
    message("⚠️ No valid selections made.")
    return(character(0))
  }

  selected_files <- matched_files[selected_indices]
  return(selected_files)
}
