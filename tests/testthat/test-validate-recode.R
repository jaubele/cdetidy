test_that("many source values may share one target value", {
  x <- data.frame(
    source = c("RB", "RB", "RE_B", "RW", "RE_W"),
    target = c(1L, 1L, 1L, 8L, 8L)
  )

  result <- validate_recode(
    x,
    source_col = "source",
    target_col = "target"
  )

  expect_true(result$pass)
  expect_equal(nrow(result$problems), 0L)
  expect_equal(nrow(result$problem_data), 0L)
})

test_that("a non-missing source may not map to a missing target", {
  x <- data.frame(
    source = c("RB", "RW"),
    target = c(NA_integer_, 8L)
  )

  expect_warning(
    result <- validate_recode(
      x,
      source_col = "source",
      target_col = "target"
    ),
    "Invalid recode"
  )

  expect_false(result$pass)
  expect_equal(result$problems$source_value, "RB")
  expect_equal(nrow(result$problem_data), 1L)
})

test_that("one source may not map to multiple target values", {
  x <- data.frame(
    source = c("RB", "RB"),
    target = c(1L, 2L)
  )

  expect_warning(
    result <- validate_recode(
      x,
      source_col = "source",
      target_col = "target"
    ),
    "Invalid recode"
  )

  expect_false(result$pass)
  expect_equal(result$problems$source_value, "RB")
  expect_equal(result$problems$n_targets, 2L)
})

test_that("fail_on_invalid stops the pipeline", {
  x <- data.frame(
    source = "RB",
    target = NA_integer_
  )

  expect_error(
    validate_recode(
      x,
      source_col = "source",
      target_col = "target",
      fail_on_invalid = TRUE
    ),
    "Invalid recode"
  )
})

test_that("missing recode columns produce a clear error", {
  x <- data.frame(source = "RB")

  expect_error(
    validate_recode(
      x,
      source_col = "source",
      target_col = "target"
    ),
    "Missing recode column"
  )
})

test_that("missing source values are ignored", {
  x <- data.frame(
    source = c(NA, "RB"),
    target = c(NA_integer_, 1L)
  )

  result <- validate_recode(
    x,
    source_col = "source",
    target_col = "target"
  )

  expect_true(result$pass)
  expect_equal(nrow(result$problems), 0L)
})
