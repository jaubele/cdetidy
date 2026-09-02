test_that("same-level CDS conflicts are resolved once and retained for QA", {
  x <- data.frame(
    cds = c("01123450000001", "01123450000001"),
    org_level = c("S", "S"),
    school_name = c("Alpha School", "Beta School")
  )

  result <- resolve_conflicting_cds(x)

  expect_equal(result$cds, c("01123450000001", "011234500000012"))
  expect_equal(result$altered_cds, c(0L, 1L))
  expect_equal(nrow(attr(result, "same_level_conflicts")), 1L)
  expect_equal(nrow(attr(result, "conflicting_cds")), 0L)
})

test_that("cross-level conflict metadata survives same-level processing", {
  x <- data.frame(
    cds = c("01123450000000", "01123450000000"),
    org_level = c("D", "S"),
    school_name = c(NA, "Example School")
  )

  result <- resolve_conflicting_cds(x)

  expect_equal(result$cds, c("01123450000000", "011234500000009999"))
  expect_equal(result$altered_cds, c(0L, 1L))
  expect_equal(nrow(attr(result, "conflicting_cds")), 1L)
})

test_that("a custom altered-column name is respected", {
  x <- data.frame(
    cds = c("01123450000000", "01123450000000"),
    org_level = c("D", "S"),
    school_name = c(NA, "Example School")
  )

  result <- resolve_conflicting_cds(
    x,
    altered_col = "cds_was_altered"
  )

  expect_equal(result$cds_was_altered, c(0L, 1L))
  expect_false("altered_cds" %in% names(result))
})
