test_that("SBAC student groups and grades use separate namespaces", {
  test_data <- data.frame(
    student_group_id = c(3L, 4L, 243L),
    grade = c(3L, 4L, 8L)
  )
  
  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L)
  
  expect_identical(
    result$student_group_id,
    test_data$student_group_id)
  
  expect_identical(
    result$grade,
    test_data$grade)
  
  expect_equal(
    result$demo_group_label,
    c(
      "Male",
      "Female",
      "Adult English Learner"))
  
  expect_equal(
    result$grade_label,
    c(
      "Grade 3",
      "Grade 4",
      "Grade 8"))
  
  expect_type(
    result$demo_group_num,
    "integer")
  
  expect_type(
    result$grade_num,
    "integer")
})


test_that("SBAC mapping fails on an unknown student-group code", {
  test_data <- data.frame(
    student_group_id = c(1L, 999L),
    grade = c(3L, 3L)
  )
  
  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = c(
        "student_group_id",
        "grade"),
      output_names = c(
        "demo_group",
        "grade"),
      variable_types = c(
        "student_group",
        "grade"),
      assessment_type = "SBAC",
      data_year = 25L),
    "unmapped value.*999")
})


test_that("SBAC mapping fails on an unknown grade", {
  test_data <- data.frame(
    student_group_id = 1L,
    grade = 99L
  )
  
  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = c(
        "student_group_id",
        "grade"),
      output_names = c(
        "demo_group",
        "grade"),
      variable_types = c(
        "student_group",
        "grade"),
      assessment_type = "SBAC",
      data_year = 25L),
    "unmapped value.*99")
})


test_that("SBAC mapping supports zero-padded character codes", {
  test_data <- data.frame(
    student_group_id = c("001", "074"),
    grade = c("03", "08")
  )
  
  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L)
  
  expect_equal(
    result$demo_group_label,
    c(
      "All Students",
      "Black or African American"))
  
  expect_equal(
    result$grade_label,
    c(
      "Grade 3",
      "Grade 8"))
})
