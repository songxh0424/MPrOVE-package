context("checkLike")

data("ICD9to10")
bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
                9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
                3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
                7300:7309,7832,78079,7808,2859)

test_that("works as intended", {
  expect_true(checkLike("B7", getICD10(bpExclICD9), ICD9to10$ICD10))
  expect_false(checkLike("C96", getICD10(bpExclICD9), ICD9to10$ICD10))
  expect_true(checkLike("C96", getICD10(c(bpExclICD9, 27789)), ICD9to10$ICD10))
})
