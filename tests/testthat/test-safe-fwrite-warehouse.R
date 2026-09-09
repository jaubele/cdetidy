test_that("safe_fwrite_warehouse writes without logging by default", {
  output_path <- tempfile(
    pattern = "safe_fwrite_warehouse_",
    fileext = ".csv")
  
  log_path <- tempfile(
    pattern = "warehouse_export_log_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      c(output_path, log_path),
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = c(
      "00123450000001",
      "00123450000002"),
    value = c(10, 20))
  
  result <- suppressMessages(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      data_year = 2025L,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "warehouse_test",
      log_path = log_path,
      n_check = 0L))
  
  expect_true(
    file.exists(output_path))
  
  expect_false(
    file.exists(log_path))
  
  expect_s3_class(
    result,
    "data.frame")
  
  expect_equal(
    nrow(result),
    1L)
  
  expect_identical(
    result$file_path,
    normalizePath(output_path))
  
  expect_identical(
    result$table_type,
    "fact")
})


test_that("safe_fwrite_warehouse honors character columns", {
  output_path <- tempfile(
    pattern = "warehouse_character_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      output_path,
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = "00123450000001",
    custom_id = "00042",
    value = 10)
  
  suppressMessages(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      char_cols = c(
        "cds",
        "custom_id"),
      data_year = 2025L,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "warehouse_character_test",
      n_check = 0L))
  
  written_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = c(
        "cds",
        "custom_id")))
  
  expect_type(
    written_data$cds,
    "character")
  
  expect_type(
    written_data$custom_id,
    "character")
  
  expect_identical(
    written_data$cds,
    test_data$cds)
  
  expect_identical(
    written_data$custom_id,
    test_data$custom_id)
})


test_that("safe_fwrite_warehouse protects existing files", {
  output_path <- tempfile(
    pattern = "warehouse_overwrite_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      output_path,
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = "00123450000001",
    value = 10)
  
  suppressMessages(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      data_year = 2025L,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "warehouse_overwrite_test",
      n_check = 0L))
  
  expect_error(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      data_year = 2025L,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "warehouse_overwrite_test",
      n_check = 0L),
    "already exists")
  
  expect_no_error(
    suppressMessages(
      safe_fwrite_warehouse(
        data = test_data,
        path = output_path,
        data_year = 2025L,
        data_source = "Assessment",
        user_note = "Temporary fact test",
        table_name = "warehouse_overwrite_test",
        n_check = 0L,
        overwrite = TRUE)))
})


test_that("safe_fwrite_warehouse creates a log only when requested", {
  output_path <- tempfile(
    pattern = "warehouse_logged_",
    fileext = ".csv")
  
  log_path <- tempfile(
    pattern = "warehouse_export_log_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      c(output_path, log_path),
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = "00123450000001",
    value = 10)
  
  result <- suppressMessages(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      data_year = 2025L,
      data_source = "Assessment",
      data_description = "Temporary warehouse export test",
      user_note = "Temporary fact test",
      table_name = "warehouse_logged_test",
      log_path = log_path,
      n_check = 0L,
      write_log = TRUE))
  
  expect_true(
    file.exists(log_path))
  
  written_log <- read.csv(
    log_path,
    stringsAsFactors = FALSE)
  
  expect_equal(
    nrow(written_log),
    1L)
  
  expect_identical(
    written_log$canonical_table_id,
    "warehouse_logged_test")
  
  expect_identical(
    result$data_description,
    "Temporary warehouse export test")
})


test_that("safe_fwrite_warehouse accepts metadata supplied as a list", {
  output_path <- tempfile(
    pattern = "warehouse_metadata_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      output_path,
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = "00123450000001",
    value = 10)
  
  result <- suppressMessages(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_path,
      log_metadata = list(
        data_year = 2025L,
        data_source = "Assessment",
        data_description = "Metadata-list test",
        user_note = "Temporary fact test"),
      table_name = "warehouse_metadata_test",
      n_check = 0L))
  
  expect_true(
    file.exists(output_path))
  
  expect_identical(
    result$data_source,
    "Assessment")
  
  expect_identical(
    result$data_description,
    "Metadata-list test")
  
  expect_identical(
    result$table_type,
    "fact")
})


test_that("safe_fwrite_warehouse requires descriptions only for logging", {
  output_without_log <- tempfile(
    pattern = "warehouse_no_description_",
    fileext = ".csv")
  
  output_with_log <- tempfile(
    pattern = "warehouse_missing_description_",
    fileext = ".csv")
  
  log_path <- tempfile(
    pattern = "warehouse_description_log_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      c(
        output_without_log,
        output_with_log,
        log_path),
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = "00123450000001",
    value = 10)
  
  expect_no_error(
    suppressMessages(
      safe_fwrite_warehouse(
        data = test_data,
        path = output_without_log,
        data_year = 2025L,
        data_source = "Assessment",
        user_note = "Temporary fact test",
        table_name = "warehouse_no_description_test",
        n_check = 0L)))
  
  expect_error(
    safe_fwrite_warehouse(
      data = test_data,
      path = output_with_log,
      data_year = 2025L,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "warehouse_missing_description_test",
      log_path = log_path,
      n_check = 0L,
      write_log = TRUE),
    "data_description")
})
