test_that("primary-key validation returns consistent problem details", {
  x <- data.frame(id = c(1L, 1L, 2L), value = letters[1:3])
  result <- validate_primary_key(x, "id", full_run = TRUE, return_problem_data = TRUE)

  expect_false(result$pass)
  expect_equal(result$duplicates, 2L)
  expect_equal(nrow(result$problem_data), 2L)
})
