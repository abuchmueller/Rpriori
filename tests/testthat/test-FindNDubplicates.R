context("Test FindNDuplicates ")
test_that("3 items, 0,1,2 and 3 duplicatd tested", {
  
  input1 <- matrix(as.logical(c(1,0,1,0,1,1,1,1,0)), nrow = 3)
  input1_sparse <- as(input1, "ngCMatrix")
  
  expect_equal(FindNDuplicates(input1_sparse), 0)
  
  
  input2 <- matrix(as.logical(c(1,1,1,1,1,0,1,1,1)), nrow = 3)
  input2_sparse <- as(input2, "ngCMatrix")
  
  expect_equal(FindNDuplicates(input2_sparse), 1)
  
  
  input3 <- matrix(as.logical(c(1,1,1,1,1,1)), nrow = 3)
  input3_sparse <- as(input3, "ngCMatrix")
  
  expect_equal(FindNDuplicates(input3_sparse), 1)
  
  
  input4 <- matrix(as.logical(c(1,0,1,1,1,1,1,1,0,1,0,1,1,0,1)), nrow = 3)
  input4_sparse <- as(input4, "ngCMatrix")
  
  expect_equal(FindNDuplicates(input4_sparse), 2)
  
  
  input5 <- matrix(as.logical(c(1,0,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1)), nrow = 3)
  input5_sparse <- as(input5, "ngCMatrix")
  
  expect_equal(FindNDuplicates(input5_sparse), 3)
  
})