context("output code and description")

str1 = "7213 72190 72210 72252 7226 72293 72402 7242-7246 72470 72471 72479 7385 7393 7394 8460-8463 8468 8469 8472 (back pain, various causes), 14xx–208xx 230xx- 239xx (cancer), 800x-839xx 850xx-854xx 86xxx 905xx-909xx 92611 92612 929, 952xx 958xx-959xx (trauma), 3040x-3042x 3044x 3054x-3057x (iv drug abuse), 34460 7292x (neurologic impairment), 4210 4211 4219 (endocarditis), 038xx (septicemia), 01xxx (tuberculosis), 730xx (osteomyelitis), 7806x 7830x 7832x 78079 7808x 2859x (fever, weight loss, malaise, night sweats, anemia not due to blood loss)"

str2 = "7802 9921 (syncope), 345xx 7803x (epilepsy or convulsions), 43xx (cerebrovascular diseases, including stroke/tia and subarachnoid hemorrhage), 800xx-804xx 850xx-854xx 870xx-873xx 9590x 910xx 920xx-921xx (head or face trauma), 78097 781xx 7820 7845x (altered mental status, nervous and musculoskeletal system symptoms, including gait abnormality, meningismus, disturbed skin sensation, speech deficits), v1254 v10xx (personal history of stroke/tia )"

str3 = "70450 70460 70470 70551- 70553"

str4 = "72010 72020 72052 72100 72110 72114 72120 72200 72202 72220 72131-72133 72141 72142 72146-72149 72156 72157 72158"

test_that("output correctly", {
  expect_equal_to_reference(getDesc(str1), file = "getDesc1.rds")
  expect_equal_to_reference(getDesc(str2), file = "getDesc2.rds")
  expect_equal_to_reference(getDesc(str3), file = "getDesc3.rds")
  expect_equal_to_reference(getDesc(str4), file = "getDesc4.rds")
})

test_that("display warning when no description available", {
  expect_warning(getDesc(str3))
  expect_warning(getDesc(str4))
})
