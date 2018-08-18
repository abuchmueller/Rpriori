context("Test Frequent Itemset")

test_that("Simple test ", {

  dataset <-  readRDS(system.file("testdata","FrequentItemsets_Itemsets.rds", package="ProjectApriori"))
  dataset <- new('TAMatrix',
                 data = as(dataset, 'ngTMatrix'),
                 dim = c(nrow(dataset), ncol(dataset)),
                 items = row.names(dataset))
  
  result <- readRDS(system.file("testdata","FrequentItemsets_FrequentItems.rds", package="ProjectApriori"))
  result <- new("FIMatrix",
                data = as(result$sets, "ngTMatrix"),
                support = result$support)
  
  
  expect_equal(FrequentItemsets(dataset, minsupport = 0.3), result)
})
