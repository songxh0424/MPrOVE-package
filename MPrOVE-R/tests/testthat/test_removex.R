context("Remove x from a vector of ICD-9 string")

test_that("works for numerical codes", {
  expect_equal(removex("7213"), "7213")
  expect_equal(removex(c("7213", "86063")), c("7213", "86063"))
  expect_equal(removex(c("7213", "33920-33922")), c("7213", "33920", "33921", "33922"))
})

test_that("works for codes with x", {
  expect_equal(removex("339xx"), "339")
  expect_equal(removex("14xx- 208xx"), as.character(140:208))
  expect_equal(removex(c("230xx-239xx", "105xx-21xx")), as.character(c(230:239, 105:219)))
})
