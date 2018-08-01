context("Test CombineCands")

test_that("Manually created test of CombineCands", {
  L1 <- as(matrix()[0,0], "ngCMatrix")
  L2 <- as(matrix(c(TRUE), dimnames = list(c("a"), NULL)), "ngCMatrix")
  L3 <- as(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2,dimnames = list(c("a", "b"), NULL)), "ngCMatrix")
  L4 <- as(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), nrow = 3,dimnames = list(c("a", "b", "c"), NULL)), "ngCMatrix")
  L5 <- as(matrix(c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE), nrow = 4, dimnames = list(c("a", "b", "c", "d"), NULL)), "ngCMatrix")
  test_list <- list(L1, L2, L3, L4, L5)
  result <- as(matrix(c(TRUE, FALSE, FALSE, FALSE,
                        TRUE, FALSE, FALSE, FALSE,
                        FALSE, TRUE, FALSE, FALSE,
                        TRUE, FALSE, TRUE, FALSE,
                        FALSE, TRUE, TRUE, FALSE,
                        TRUE, TRUE, TRUE, FALSE,
                        TRUE, FALSE, FALSE, TRUE ), nrow = 4, dimnames = list(c("a", "b", "c", "d"), NULL)), "ngCMatrix")
  CombineCands(test_list)
  CombineCands
  
  expect_equal(CombineCands(test_list), result)
}) 
