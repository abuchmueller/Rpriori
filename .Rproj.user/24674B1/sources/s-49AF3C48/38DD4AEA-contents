context("Unit testing of the function GenCandidates that does generate Candidates for frequent itemset and rule mining")


test_that("Test of the example from wikipedia", {
  ##### TEST: AprioriGen #####
  # In this test I will use the example from wikipedia #
  # Testing set-up based on example from wikipedia #
  # This does represent the example candidate set from wikipedia "https://de.wikipedia.org/wiki/Apriori-Algorithmus" in a incident matrix #
  testmat <- matrix(as.logical(c(1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0)), nrow = 5, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL))
  
  # The result should be #
  result_mat <- matrix(c(TRUE, TRUE, TRUE, TRUE, FALSE), ncol = 1, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL))
  
  expect_equal(result_mat,GenCandidates(testmat))

})

