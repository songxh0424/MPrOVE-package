context("createLIKE and convertToLike")

data("ICD9to10")
bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
                9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
                3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
                7300:7309,7832,78079,7808,2859)
bpExclICD10 <- getICD10(bpExclICD9)

test_that("results are the same as the results stored in an RDS file", {
  expect_equal_to_reference(createLIKE(codes = bpExclICD10, dict = ICD9to10$ICD10), 
                            file = "create.rds")
  expect_equal_to_reference(convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10, maxLength = 4), 
                            file = "convert.rds")
})

test_that("codes that share few similar leading substrings (less than minGroup)
          don't cause errors", {
  teststr1 <- c("A001", "B951", "C269")
  teststr2 <- c("A001", "A000", "A009", "A0100")
  expect_error(createLIKE(codes = teststr1, dict = ICD9to10$ICD10), NA)
  expect_error(convertToLike(codes = teststr1, dict = ICD9to10$ICD10), NA)
  expect_error(createLIKE(codes = teststr2, dict = ICD9to10$ICD10, 
                          minGroup = 5), NA)
  expect_error(convertToLike(codes = teststr2, dict = ICD9to10$ICD10, 
                             minGroup = 5), NA)
})
