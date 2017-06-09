context("add periods in ICD codes")

test_that("correctly add periods after the third letter", {
  expect_equal(addPeriods("123456"), "123.456")
  expect_equal(addPeriods("A23456"), "A23.456")
  expect_equal(addPeriods(c("1234", "A23456")), c("123.4", "A23.456"))
})
