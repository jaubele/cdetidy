test_that("variable comparison returns structured results", {
  old <- data.frame(a = 1, b = 2)
  new <- data.frame(a = 1, c = 3)

  result <- compare_variable_names(old, new)
  expect_false(result$identical)
  expect_equal(result$only_previous[[1]], "b")
  expect_equal(result$only_current[[1]], "c")
})

test_that("suppression comparison returns structured results", {
  old <- data.frame(a = c("*", "1"), b = c("*", "2"))
  new <- data.frame(a = c("*", "1"), c = c("*", "3"))

  result <- compare_suppression_columns(old, new)
  expect_equal(result$new_suppressed_cols[[1]], "c")
  expect_equal(result$stopped_being_suppressed[[1]], "b")
  expect_equal(result$unchanged_suppressed_cols[[1]], "a")
})
