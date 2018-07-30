context("Test Frequent Itemset")

test_that("Simple test ", {
  
  dataset <-  readRDS(system.file("testdata","FrequentItemsets_Itemsets.rds", package="ProjectApriori"))
  result <- readRDS(system.file("testdata","FrequentItemsets_FrequentItems.rds", package="ProjectApriori")) 
  expect_equal(FrequentItemsets(dataset, minsupport = 0.3), result)
})
