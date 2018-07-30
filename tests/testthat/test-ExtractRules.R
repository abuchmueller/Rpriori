context("Test ExtractRules")

test_that("test with consequents of one", {
  input <- readRDS(system.file("testdata","ExtractRules_R1.rds", package="ProjectApriori"))
  result <- readRDS(system.file("testdata","ExtractRules_result1.rds", package="ProjectApriori"))
  expect_equal(ExtractRules(input), result)
})

test_that("test with consequents of two",{
  input <- readRDS(system.file("testdata","ExtractRules_R2.rds", package="ProjectApriori"))
  result <- readRDS(system.file("testdata","ExtractRules_result2.rds", package="ProjectApriori"))
  expect_equal(ExtractRules(input), result)
})


