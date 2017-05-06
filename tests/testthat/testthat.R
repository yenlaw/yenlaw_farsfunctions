# devtools::test()
library(testthat)
library(farsfunctions)

#context("Incorrect Data")
#test_check("farsfunctions")

test_that("str_length of missing is missing", {
  expect_warning(fars_read_years(2017), "invalid year: 2017")
})
