context("convert2regex")

data("ICD9to10")
bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
                9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
                3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
                7300:7309,7832,78079,7808,2859)
bpExclICD10 <- getICD10(bpExclICD9)
codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)
nm <- names(codeList$LIKE)

test_that("results are the same as the results stored in an RDS file", {
  expect_equal_to_reference(convert2regex(nm,'C',colName = 'DX_CD'), 
                            file = "toregex1.rds")
  expect_equal_to_reference(convert2regex(nm,'D',colName = 'DX_CD'), 
                            file = "toregex2.rds")
})
