test_that("many source codes may share one standardized value", {
  x <- data.frame(
    source = c("RB", "RE_B", "RW", "RE_W"),
    target = c(1L, 1L, 8L, 8L)
  )

  result <- validate_group_mapping_tabyl(x, source, target)
  expect_true(result)
  expect_equal(nrow(attr(result, "problems")), 0L)
})

test_that("one source code may not map to multiple standardized values", {
  x <- data.frame(source = c("RB", "RB"), target = c(1L, 2L))

  result <- validate_group_mapping_tabyl(x, source, target)
  expect_false(result)
  expect_equal(attr(result, "problems")$source, "RB")
})

test_that("a nonmissing source may not have a missing target", {
  x <- data.frame(source = "RB", target = NA_integer_)
  expect_false(validate_group_mapping_tabyl(x, source, target))
})
