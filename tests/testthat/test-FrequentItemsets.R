context("Test Frequent Itemset")

test_that("Simple test ", {

  FrequentItemsets_Itemsets <-  Rpriori:::FrequentItemsets_Itemsets
  dataset <- new('TAMatrix',
                 data = as(FrequentItemsets_Itemsets, 'ngTMatrix'),
                 dim = c(nrow(FrequentItemsets_Itemsets), ncol(FrequentItemsets_Itemsets)),
                 items = row.names(FrequentItemsets_Itemsets))
  
  FrequentItemsets_FrequentItems <- Rpriori:::FrequentItemsets_FrequentItems
  result <- new("FIMatrix",
                data = as(FrequentItemsets_FrequentItems$sets, "ngTMatrix"),
                support = FrequentItemsets_FrequentItems$support)

  expect_equal(FindFrequentItemsets(dataset, minsupport = 0.3), result)
})
