context("Test DetSupport")

testthat::test_that("Empty input, Empty output",{
 
  cand <- readRDS(system.file("testdata","Test_GiveUniquecols_L3.rds", package="ProjectApriori"))  
  
  testthat::expect_equal(DetSupport(cand, NULL), c(0)[0])
})

testthat::test_that("Simple test of DetSupport_row with candidates that have different amount of items but all items as rows",{
  # A candidate containing 4 items #
  cand1 <-  matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand1 <- as(cand1, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand2 <- matrix(c(TRUE, FALSE,  TRUE, FALSE, FALSE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand2 <- as(cand2, 'ngTMatrix')
  
  # A candidate containing 5 items #
  cand3 <- matrix(c(TRUE, TRUE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand3 <- as(cand3, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand4 <- matrix(c(TRUE, FALSE,  FALSE, FALSE ,FALSE),
                  dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand4 <- as(cand4, 'ngTMatrix')
  
  # Here I will create the transaction database #
  trans <- new('TAMatrix',
               data = as(matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE, 
                    TRUE, FALSE,TRUE, FALSE, FALSE,
                    TRUE, FALSE,  TRUE, TRUE, FALSE,
                    FALSE, TRUE, FALSE, FALSE, TRUE,
                    FALSE, FALSE, TRUE, FALSE, FALSE,
                    TRUE, TRUE, TRUE, TRUE, TRUE,
                    TRUE, FALSE,  TRUE, TRUE, FALSE,
                    TRUE, FALSE, TRUE, FALSE, TRUE,
                    TRUE, FALSE, TRUE, FALSE, TRUE,
                    TRUE, FALSE, TRUE, TRUE, TRUE),
                    dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), nrow = 5),
                    'ngTMatrix'),
                dim = c(5L, 5L),
               items = c('a', 'b', 'c', 'd', 'e'))
  
  testthat::expect_equal(DetSupport(cand1, trans), 0.3)
  testthat::expect_equal(DetSupport(cand2, trans), 0.8)
  testthat::expect_equal(DetSupport(cand3, trans), 0.1)
  testthat::expect_equal(DetSupport(cand4, trans), 0.8)
})


testthat::test_that("Simple test of DetSupport_row with candidates that have different amount of items and different rows",{
  # A candidate containing 4 items #
  cand1 <-  matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand1 <- as(cand1, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand2 <- matrix(c(TRUE, FALSE,  TRUE, FALSE, FALSE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand2 <- as(cand2, 'ngTMatrix')
  
  # A candidate containing 5 items #
  cand3 <- matrix(c(TRUE, TRUE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand3 <- as(cand3, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand4 <- matrix(c(TRUE, FALSE,  FALSE, FALSE ,FALSE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand4 <- as(cand4, 'ngTMatrix')
  
  # Here I will create the transaction database #
  trans <- new('TAMatrix',
               data = as(matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE, 
                                  TRUE, FALSE,TRUE, FALSE, FALSE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  FALSE, TRUE, FALSE, FALSE, TRUE,
                                  FALSE, FALSE, TRUE, FALSE, FALSE,
                                  TRUE, TRUE, TRUE, TRUE, TRUE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, TRUE, TRUE),
                                dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), nrow = 5),
                         'ngTMatrix'),
               dim = c(5L, 5L),
               items = c('a', 'b', 'c', 'd', 'e'))
  
  testthat::expect_equal(DetSupport(cand1, trans), 0.3)
  testthat::expect_equal(DetSupport(cand2, trans), 0.8)
  testthat::expect_equal(DetSupport(cand3, trans), 0.1)
  testthat::expect_equal(DetSupport(cand4, trans), 0.8)
})





testthat::test_that("Simple test of DetSupport  with candidates that have different amount of items and the same amount of  rows",{
  # A candidate containing 4 items #
  cand1 <-  matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand1 <- as(cand1, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand2 <- matrix(c(TRUE, FALSE,  TRUE, FALSE, FALSE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand2 <- as(cand2, 'ngTMatrix')
  
  # A candidate containing 5 items #
  cand3 <- matrix(c(TRUE, TRUE,  TRUE, TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand3 <- as(cand3, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand4 <- matrix(c(TRUE, FALSE,  FALSE, FALSE ,FALSE), dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), ncol = 1)
  cand4 <- as(cand4, 'ngTMatrix')
  
  # Here I will create the transaction database #
  trans <- new('TAMatrix',
               data = as(matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE, 
                                  TRUE, FALSE,TRUE, FALSE, FALSE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  FALSE, TRUE, FALSE, FALSE, TRUE,
                                  FALSE, FALSE, TRUE, FALSE, FALSE,
                                  TRUE, TRUE, TRUE, TRUE, TRUE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, TRUE, TRUE),
                                dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), nrow = 5),
                         'ngTMatrix'),
               dim = c(5L, 5L),
               items = c('a', 'b', 'c', 'd', 'e'))
  
  
  testthat::expect_equal(DetSupport(cbind(cand1,cand2, cand3, cand4), trans),c(0.3 ,0.8 ,0.1 ,0.8))
})


testthat::test_that("Simple test of DetSupport  with candidates that have different amount of items and the different amount  of  rows",{
  # A candidate containing 4 items #
  cand1 <-  matrix(c(TRUE, FALSE,  TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd'), NULL), ncol = 1)
  cand1 <- as(cand1, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand2 <- matrix(c(TRUE, FALSE,  TRUE, FALSE), dimnames = list(c('a', 'b', 'c', 'd'), NULL), ncol = 1)
  cand2 <- as(cand2, 'ngTMatrix')
  
  # A candidate containing 5 items #
  cand3 <- matrix(c(TRUE, TRUE,  TRUE, TRUE), dimnames = list(c('a', 'b', 'c', 'd'), NULL), ncol = 1)
  cand3 <- as(cand3, 'ngTMatrix')
  
  # A candidate containing 2 items #
  cand4 <- matrix(c(TRUE, FALSE,  FALSE, FALSE), dimnames = list(c('a', 'b', 'c', 'd'), NULL), ncol = 1)
  cand4 <- as(cand4, 'ngTMatrix')
  
  # Here I will create the transaction database #
  trans <- new('TAMatrix',
               data = as(matrix(c(TRUE, FALSE,  TRUE, TRUE, TRUE, 
                                  TRUE, FALSE,TRUE, FALSE, FALSE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  FALSE, TRUE, FALSE, FALSE, TRUE,
                                  FALSE, FALSE, TRUE, FALSE, FALSE,
                                  TRUE, TRUE, TRUE, TRUE, TRUE,
                                  TRUE, FALSE,  TRUE, TRUE, FALSE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, TRUE, TRUE),dimnames = list(c('a', 'b', 'c', 'd', 'e'), NULL), nrow = 5),
                         'ngTMatrix'),
               dim = c(5L, 5L),
               items = c('a', 'b', 'c', 'd', 'e'))
  
  testthat::expect_equal(DetSupport(cbind(cand1,cand2, cand3, cand4), trans),c(0.5 ,0.8 ,0.1 ,0.8))
})