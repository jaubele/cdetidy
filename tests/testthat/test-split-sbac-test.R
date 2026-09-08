test_that("split_sbac_test separates ELA and Math", {
  test_data <- data.frame(
    row_id = 1:6,
    test = c(1L, 2L, 1L, 2L, 1L, 2L),
    value = letters[1:6])
  
  result <- split_sbac_test(test_data)
  
  expect_named(
    result,
    c("ela", "math"))
  
  expect_equal(
    result$ela$row_id,
    c(1L, 3L, 5L))
  
  expect_equal(
    result$math$row_id,
    c(2L, 4L, 6L))
  
  expect_true(
    all(result$ela$test == 1L))
  
  expect_true(
    all(result$math$test == 2L))
  
  expect_equal(
    nrow(result$ela) + nrow(result$math),
    nrow(test_data))
})


test_that("split_sbac_test preserves columns and within-subject order", {
  test_data <- data.frame(
    row_id = c(4L, 1L, 3L, 2L),
    test = c(2L, 1L, 2L, 1L),
    value = c("d", "a", "c", "b"))
  
  result <- split_sbac_test(test_data)
  
  expect_identical(
    names(result$ela),
    names(test_data))
  
  expect_identical(
    names(result$math),
    names(test_data))
  
  expect_equal(
    result$ela$row_id,
    c(1L, 2L))
  
  expect_equal(
    result$math$row_id,
    c(4L, 3L))
})


test_that("split_sbac_test requires a test column", {
  test_data <- data.frame(
    value = 1:3)
  
  expect_error(
    split_sbac_test(test_data),
    "must include a `test` column")
})


test_that("split_sbac_test rejects missing test values", {
  test_data <- data.frame(
    test = c(1L, 2L, NA_integer_))
  
  expect_error(
    split_sbac_test(test_data),
    "contains missing values")
})


test_that("split_sbac_test rejects unexpected test codes", {
  test_data <- data.frame(
    test = c(1L, 2L, 3L))
  
  expect_error(
    split_sbac_test(test_data),
    "Unexpected.*3")
})


test_that("split_sbac_test requires both subjects", {
  ela_only <- data.frame(
    test = rep(1L, 3L))
  
  math_only <- data.frame(
    test = rep(2L, 3L))
  
  expect_error(
    split_sbac_test(ela_only),
    "No Math records")
  
  expect_error(
    split_sbac_test(math_only),
    "No ELA records")
})


test_that("split_sbac_test does not create global objects", {
  test_data <- data.frame(
    test = c(1L, 2L))
  
  names_before <- ls(
    envir = .GlobalEnv,
    all.names = TRUE)
  
  result <- split_sbac_test(test_data)
  
  names_after <- ls(
    envir = .GlobalEnv,
    all.names = TRUE)
  
  expect_identical(
    names_after,
    names_before)
  
  expect_named(
    result,
    c("ela", "math"))
})
