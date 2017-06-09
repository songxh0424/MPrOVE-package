context("writeSQL")

data("ICD9to10")
bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
                9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
                3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
                7300:7309,7832,78079,7808,2859)
bpExclICD10 <- getICD10(bpExclICD9)
codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)

test_that("string output works as intended", {
  expect_is(writeSQL(codeList,'DX_CD', write.file = FALSE), "character")
  expect_equal(length(writeSQL(codeList,'DX_CD', write.file = FALSE)), 1)
  expect_equal_to_reference(writeSQL(codeList,'DX_CD', write.file = FALSE), 
                            file = "string.rds")
})
