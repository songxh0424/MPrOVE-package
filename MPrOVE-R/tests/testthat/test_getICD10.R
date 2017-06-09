context("ICD9 to ICD10 codes conversion")

test_that("getICD10 converts individual code correctly", {
  expect_equal(getICD10(78097), "R4182")
  expect_equal(getICD10(71741), "M23202")
  expect_equal(getICD10(43411), "I6340")
})

test_that("getICD10 converts range of codes correctly", {
  expect_true(setequal(getICD10(280), getICD10(c(2800:2809, 28000:28099))))
  expect_true(setequal(getICD10(2017), c("C8130", "C8139", "C8131", "C8132", "C8133", "C8134", "C8135", "C8136", "C8137", "C8138")))
  expect_true(setequal(getICD10(7159), c("M159", "M1990", "M189", "M169", "M179")))
})

test_that("correctly returns records when set return.value='All'", {
  expect_is(getICD10(4530, return.value = 'All'), "data.frame")
  expect_equal(names(getICD10(4530, return.value = 'All')), c("ICD9", "ICD10", "Flags"))
})
