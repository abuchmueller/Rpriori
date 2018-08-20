context("Test CombineCands")

test_that("Manually created test of CombineCands", {
  L1 <- new('FIMatrix',
            data = as(matrix()[0,0], "ngTMatrix"),
            support = c(1)[0])
  
  L2 <- new('FIMatrix',
            data = as(matrix(c(TRUE), dimnames = list(c("a"), NULL)), "ngTMatrix"),
            support = c(0.2))
  
  L3 <- new('FIMatrix',
            data =  as(matrix(c(TRUE, FALSE, FALSE, TRUE),
                              nrow = 2,dimnames = list(c("a", "b"), NULL)), "ngTMatrix"),
            support = c(0.1,0.4))
  
  L4 <- new('FIMatrix',
            data = as(matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
                             nrow = 3,dimnames = list(c("a", "b", "c"), NULL)), "ngTMatrix"),
            support = c(0.1,0.4))
  
  L5 <- new('FIMatrix',
            data = as(matrix(c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
                             nrow = 4, dimnames = list(c("a", "b", "c", "d"), NULL)), "ngTMatrix"),
            support = c(0.1,0.34))
  
  test_list <- list(L1, L2, L3, L4, L5)
  
  result <- new('FIMatrix',
                data =  as(matrix(c(TRUE, FALSE, FALSE, FALSE,
                                    TRUE, FALSE, FALSE, FALSE,
                                    FALSE, TRUE, FALSE, FALSE,
                                    TRUE, FALSE, TRUE, FALSE,
                                    FALSE, TRUE, TRUE, FALSE,
                                    TRUE, TRUE, TRUE, FALSE,
                                    TRUE, FALSE, FALSE, TRUE ),
                                  nrow = 4, dimnames = list(c("a", "b", "c", "d"), NULL)),
                           "ngTMatrix"),
                support = c(0.2, 0.1, 0.4, 0.1,0.4,0.1,0.34))
  

  expect_equal(CombineFIMatrix(test_list), result)
}) 
