test_that("safe_fwrite writes without logging by default", {
  output_path <- tempfile(
    pattern = "safe_fwrite_",
    fileext = ".csv")
  
  log_path <- tempfile(
    pattern = "safe_fwrite_log_",
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
    safe_fwrite(
      data = test_data,
      path = output_path,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "safe_fwrite_test",
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


test_that("safe_fwrite preserves identifier columns as character", {
  output_path <- tempfile(
    pattern = "safe_fwrite_character_",
    fileext = ".csv")
  
  on.exit(
    unlink(
      output_path,
      force = TRUE),
    add = TRUE)
  
  test_data <- data.frame(
    cds = c(
      "00123450000001",
      "00123450000002"),
    county_code = c(
      "01",
      "02"),
    value = c(10, 20))
  
  suppressMessages(
    safe_fwrite(
      data = test_data,
      path = output_path,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "safe_fwrite_character_test",
      n_check = 0L))
  
  written_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = c(
        "cds",
        "county_code")))
  
  expect_type(
    written_data$cds,
    "character")
  
  expect_type(
    written_data$county_code,
    "character")
  
  expect_identical(
    written_data$cds,
    test_data$cds)
  
  expect_identical(
    written_data$county_code,
    test_data$county_code)
})


test_that("safe_fwrite protects existing files", {
  output_path <- tempfile(
    pattern = "safe_fwrite_overwrite_",
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
    safe_fwrite(
      data = test_data,
      path = output_path,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "safe_fwrite_overwrite_test",
      n_check = 0L))
  
  expect_error(
    safe_fwrite(
      data = test_data,
      path = output_path,
      data_source = "Assessment",
      user_note = "Temporary fact test",
      table_name = "safe_fwrite_overwrite_test",
      n_check = 0L),
    "already exists")
  
  expect_no_error(
    suppressMessages(
      safe_fwrite(
        data = test_data,
        path = output_path,
        data_source = "Assessment",
        user_note = "Temporary fact test",
        table_name = "safe_fwrite_overwrite_test",
        n_check = 0L,
        overwrite = TRUE)))
})


test_that("safe_fwrite creates a log only when requested", {
  output_path <- tempfile(
    pattern = "safe_fwrite_logged_",
    fileext = ".csv")
  
  log_path <- tempfile(
    pattern = "safe_fwrite_log_",
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
    safe_fwrite(
      data = test_data,
      path = output_path,
      data_year = 2025L,
      data_source = "Assessment",
      data_description = "Temporary local export test",
      user_note = "Temporary fact test",
      table_name = "safe_fwrite_logged_test",
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
    "safe_fwrite_logged_test")
  
  expect_identical(
    result$data_description,
    "Temporary local export test")
})


test_that("safe_fwrite accepts metadata supplied as a list", {
  output_path <- tempfile(
    pattern = "safe_fwrite_metadata_",
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
    safe_fwrite(
      data = test_data,
      path = output_path,
      log_metadata = list(
        data_year = 2025L,
        data_source = "Assessment",
        data_description = "Metadata-list test",
        user_note = "Temporary fact test"),
      table_name = "safe_fwrite_metadata_test",
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
