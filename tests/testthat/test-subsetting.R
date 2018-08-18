context("Test Subsetting")

test_that("Test subsetting of TAMatrix", {
  
  testmat <- new('TAMatrix',
                 data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                  nrow = 3,
                                  byrow = TRUE,
                                  dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 dim = c(3L,4L),
                 items = c("a", 'b', 'c'))
  
  expect_equal(testmat[1:2,1],new('TAMatrix',
                                  data = as(matrix(c(1,0),
                                                   nrow = 2,
                                                   byrow = TRUE,
                                                   dimnames = list(c('a', 'b'),NULL)), "ngTMatrix"),
                                  dim = c(2L,1L),
                                  items = c("a", 'b')) )
  
  expect_equal(testmat[c(1,3),],new('TAMatrix',
                                    data = as(matrix(c(1,0,1,0,1,1,1,0),
                                                     nrow = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c('a', 'c'),NULL)), "ngTMatrix"),
                                    dim = c(2L,4L),
                                    items = c("a", 'c')))
  
  expect_equal(testmat[,c(TRUE, TRUE, FALSE, FALSE)],new('TAMatrix',
                                                         data = as(matrix(c(1,0,0,0,1,1),
                                                                          nrow = 3,
                                                                          byrow = TRUE,
                                                                          dimnames = list(c('a', 'b','c'),NULL)), "ngTMatrix"),
                                                         dim = c(3L,2L),
                                                         items = c("a", 'b', 'c')) )
  
})

test_that("Test subsetting of FIMatrix",{
  testmat <- new('FIMatrix',
                 data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                  nrow = 3,
                                  byrow = TRUE,
                                  dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 support = c(0.4, 0.5, 0.6, 0.7))
  
  expect_equal(testmat[1,3], new('FIMatrix',
                                 data = as(matrix(c(1),
                                                  nrow = 1,
                                                  byrow = TRUE,
                                                  dimnames = list(c('a'),NULL)), "ngTMatrix"),
                                 support = c( 0.6)))
})

