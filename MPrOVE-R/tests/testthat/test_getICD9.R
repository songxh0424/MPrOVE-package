context("ICD10 to ICD9 codes conversion")

test_that("getICD9 converts individual code correctly", {
  expect_equal(getICD9("K635"), "2113")
  expect_equal(getICD9("D2920"), "2220")
  expect_true(setequal(getICD9("T2039XA"), c("94139", "94149")))
})

test_that("getICD9 converts range of codes correctly", {
  expect_true(setequal(getICD9("A18"), getICD9(paste("A", c(180:189, 1800:1899, 18000:18999), sep = ""))))
  expect_true(setequal(getICD9("M15"), c("71500", "71504", "71509", "71580", "71589", "71590")))
  expect_true(setequal(getICD9("T634"), c("9895", "E9054", "V5889", "9091", "E9292", "E9509", "E959",  
                                          "E9621", "E969", "E9809", "E989", "E9055", "E9053")))
})

test_that("correctly returns records when set return.value='All'", {
  expect_is(getICD9("T634", return.value = 'All'), "data.frame")
  expect_equal(names(getICD10("T634", return.value = 'All')), c("ICD9", "ICD10", "Flags"))
  expect_equal(nrow(getICD9("T634", return.value = 'All')), 140)
})
