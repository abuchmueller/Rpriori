context("Test GenCandidates")

test_that("Test of the example from wikipedia", {
  ##### TEST: AprioriGen #####
  # In this test I will use the example from wikipedia #
  # Testing set-up based on example from wikipedia #
  # This does represent the example candidate set from wikipedia "https://de.wikipedia.org/wiki/Apriori-Algorithmus" in a incident matrix #
  testmat <- as(matrix(as.logical(c(1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0)), nrow = 5, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL)), 'ngTMatrix')
  # The result should be #
  result_mat <- as(matrix(c(TRUE, TRUE, TRUE, TRUE, FALSE), ncol = 1, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL)), 'ngTMatrix')
  expect_equal(result_mat,GenCandidates(testmat))
})

test_that("input itemsets of length 2 output should be empty.", {
  input <- as(matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),ncol = 3, dimnames = list(c('a', 'b', 'c', 'd'),NULL)), 'ngTMatrix')
  expect_equal(GenCandidates(input), input[,0, drop = FALSE])
})

test_that("input 2 itemsets, output 1 itemset", {
  input <- as(matrix(as.logical(c(1,0,0,0,0,
                       0,1,0,0,0)),ncol = 2, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL)), 'ngTMatrix')
  result <-  as(matrix(as.logical(c(1,1,0,0,0)),ncol = 1, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL)), 'ngTMatrix')
  expect_equal(GenCandidates(input), result)
})


