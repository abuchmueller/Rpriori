context("Test GiveUniqueCol")
test_that("3 items, all and now columns duplicates tested", {
  input1 <- matrix(as.logical(c(1,0,1,0,1,1,1,1,0)), nrow = 3)
  input1_sparse <- as(input1, "ngCMatrix")
  res1 <- input1
  
  expect_equal(as(res1, 'ngCMatrix'), GiveUniqueCol(input1_sparse))
  
  
  input2 <- matrix(as.logical(c(1,1,1,1,1,0,1,1,1)), nrow = 3)
  input2_sparse <- as(input2, "ngCMatrix")
  res2 <- matrix(as.logical(c(1,1,0,1,1,1)), nrow = 3)
  
  expect_equal(as(res2, 'ngCMatrix'), GiveUniqueCol(input2_sparse))
  
  
  input3 <- matrix(as.logical(c(1,1,1,1,1,1)), nrow = 3)
  input3_sparse <- as(input3, "ngCMatrix")
  res3 <- matrix(as.logical(c(1,1,1)), nrow = 3)
  
  expect_equal(as(res3, 'ngCMatrix'), GiveUniqueCol(input3_sparse))
  
  
  input4 <- matrix(as.logical(c(1,0,1,1,1,1,1,1,0,1,0,1,1,0,1)), nrow = 3)
  input4_sparse <- as(input4, "ngCMatrix")
  res4 <- matrix(as.logical(c(1,1,1,1,1,0,1,0,1)), nrow = 3)
  
  expect_equal(as(res4, 'ngCMatrix'), GiveUniqueCol(input4_sparse))
})