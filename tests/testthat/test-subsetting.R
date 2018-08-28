context("Test Subsetting")

test_that("Test subsetting of TAMatrix", {
  
  testmat <- new('TAMatrix',
                 data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                  nrow = 3,
                                  byrow = TRUE,
                                  dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 dim = c(3L,4L),
                 items = c("a", 'b', 'c'))
  
  expect_equal(select(testmat,1:2,1),new('TAMatrix',
                                  data = as(matrix(c(1,0),
                                                   nrow = 2,
                                                   byrow = TRUE,
                                                   dimnames = list(c('a', 'b'),NULL)), "ngTMatrix"),
                                  dim = c(2L,1L),
                                  items = c("a", 'b')) )
  
  expect_equal(select(testmat,c(1,3),),new('TAMatrix',
                                    data = as(matrix(c(1,0,1,0,1,1,1,0),
                                                     nrow = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c('a', 'c'),NULL)), "ngTMatrix"),
                                    dim = c(2L,4L),
                                    items = c("a", 'c')))
  
  expect_equal(select(testmat,,c(TRUE, TRUE, FALSE, FALSE)),new('TAMatrix',
                                                         data = as(matrix(c(1,0,0,0,1,1),
                                                                          nrow = 3,
                                                                          byrow = TRUE,
                                                                          dimnames = list(c('a', 'b','c'),NULL)), "ngTMatrix"),
                                                         dim = c(3L,2L),
                                                         items = c("a", 'b', 'c')) )
  expect_error(select(testmat,,c(TRUE, TRUE, TRUE, TRUE, FALSE)),
               'Logical subscript of length 5 too long for TAMatrix with 4 columns')
  expect_error(select(testmat,c(5, 6)))
  expect_error(select(testmat,c(TRUE, TRUE, TRUE, TRUE, FALSE),),
               'Logical subscript of length 5 too long for TAMatrix with 3 rows')
  expect_error(testmat[c(5),])

})

test_that("Test subsetting of FIMatrix",{
  testmat <- new('FIMatrix',
                 data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                  nrow = 3,
                                  byrow = TRUE,
                                  dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 support = c(0.4, 0.5, 0.6, 0.7))
  
  expect_equal(select(testmat,1,3), new('FIMatrix',
                                 data = as(matrix(c(1),
                                                  nrow = 1,
                                                  byrow = TRUE,
                                                  dimnames = list(c('a'),NULL)), "ngTMatrix"),
                                 support = c( 0.6)))
  expect_error(select(testmat,,c(TRUE, TRUE, TRUE, TRUE, FALSE)),
               'Logical subscript of length 5 too long for FIMatrix with 4 columns')
  expect_error(select(testmat,,c(5, 6)))
  expect_error(select(testmat,c(TRUE, TRUE, TRUE, TRUE, FALSE),),
               'Logical subscript of length 5 too long for FIMatrix with 3 rows')
  expect_error(select(testmat,c(5),))
})

test_that("Test subsetting of Rules",{
  testmat <- new('Rules',
                 lhs = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                  nrow = 3,
                                  byrow = TRUE,
                                  dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 rhs = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                 nrow = 3,
                                 byrow = TRUE,
                                 dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                 support = c(0.4, 0.5, 0.6, 0.7),
                 confidence = c(0.4, 0.5, 0.6, 0.7),
                 lift = c(0.4, 0.5, 0.6, 0.7),
                 leverage = c(0.4, 0.5, 0.6, 0.7),
                 itemsetID = c(0.4, 0.5, 0.6, 0.7),
                 FrequentItemsets = new("FIMatrix",
                                        data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                             nrow = 3,
                                             byrow = TRUE,
                                             dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                                        support = rep(0.3, 4)))


  expect_equal(select(testmat,1,3), new('Rules',
                                  lhs = as(matrix(c(1),
                                                  nrow = 1,
                                                  byrow = TRUE,
                                                  dimnames = list(c('a'),NULL)), "ngTMatrix"),
                                 rhs = as(matrix(c(1),
                                                 nrow = 1,
                                                 byrow = TRUE,
                                                 dimnames = list(c('a'),NULL)), "ngTMatrix"),
                                 support = c( 0.6),
                                 confidence = c( 0.6),
                                 lift = c( 0.6),
                                 leverage = c( 0.6),
                                 itemsetID = c( 0.6),
                                 FrequentItemsets = new("FIMatrix",
                                                        data = as(matrix(c(1,0,1,0,0,0,1,0,1,1,1,0),
                                                                         nrow = 3,
                                                                         byrow = TRUE,
                                                                         dimnames = list(c('a', 'b', 'c'),NULL)), "ngTMatrix"),
                                                        support = rep(0.3, 4))))
  expect_error(select(testmat,,c(TRUE, TRUE, TRUE, TRUE, FALSE)),
               'Logical subscript of length 5 too long for Rules with 4 columns')
  expect_error(select(testmat,,c(5, 6)))
  expect_error(select(testmat,c(TRUE, TRUE, TRUE, TRUE, FALSE),),
               'Logical subscript of length 5 too long for Rules with 3 rows')
  expect_error(select(testmat,c(5),))
  
})
