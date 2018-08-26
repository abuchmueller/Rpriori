context("Test of the Coercing methods for TAMatrix and FIMatrix")

test_that("Test for coercion to TAMatrix", {
  
  
  # Test data is #
  input_sets <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                         TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                         FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
                         TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                         TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6,
                       dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))
  
  # transaction object from arules
  #input_transactions <- as(t(input_sets),"transactions")
  
  # data frame
  input_sets_dataframe <- as.data.frame(input_sets)
  
  # Logical, sparse, non-compressed matrix
  input_sets_lgTMatrix <- as(input_sets, "TsparseMatrix")
  
  # logical, sparse, compressed matrix
  input_sets_lgCMatrix <- as(input_sets, "CsparseMatrix")
  
  # pattern, sparse, non-compressed matrix
  input_sets_ngTMatrix <- as(input_sets, "ngTMatrix")
  
  # pattern, sparse, compressed matrix 
  input_sets_ngCMatrix <-as(input_sets, "ngCMatrix")
  
  
  # Result #
  result <- new("TAMatrix",
                data = as(input_sets, 'ngTMatrix'),
                dim = c(nrow(input_sets), ncol(input_sets)),
                items = rownames(input_sets))
  
  
  # From Matrix #
  dat <- makeTAMatrix(input_sets)
  expect_equal(dat, result)
  
  # from class transactions
  #dat_transaction <- makeTAMatrix(input_transactions)
  #expect_equal(dat_transaction, result)
  
  # from dataframe
  res_dataframe <- makeTAMatrix(input_sets_dataframe)
  expect_equal(res_dataframe, result)
  
  # from logical, non-compressed, sparse matrix
  dat_lgTMatrix <- makeTAMatrix(input_sets_lgTMatrix)
  expect_equal(dat_lgTMatrix, result)
  
  # from logical, compressed, sparse matrix
  dat_lgCMatrix <- makeTAMatrix(input_sets_lgCMatrix)
  expect_equal(dat_lgCMatrix, result)
  
  # from pattern, non-compressed, sparse matrix
  dat_ngCMatrix <- makeTAMatrix(input_sets_ngTMatrix)
  expect_equal(dat_ngCMatrix, result)
  
  # from pattern, compressed, sparse matrix
  dat_ngCMatrix <- makeTAMatrix(input_sets_ngCMatrix)
  expect_equal(dat_ngCMatrix, result)
  
})



test_that("Test for coercion to FIMatrix",{
  
  dataset <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                      TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                      FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
                      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                      TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6,
                    dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))
  
  Frequent_items <- matrix(c(TRUE, FALSE, FALSE, FALSE,  TRUE,  TRUE, FALSE, FALSE,
                             FALSE,  TRUE, FALSE, FALSE,  TRUE, FALSE,  TRUE, FALSE,
                             FALSE, FALSE,  TRUE, FALSE, FALSE,  TRUE,  TRUE,  TRUE,
                             FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE),
                           nrow = 4,
                           byrow = TRUE,
                           dimnames = list( c("Bread", "Milk", "Diaper", "Beer"),NULL))
  
  Frequent_items_support <- c(0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.6)
    
    result <- new('FIMatrix',
                  data = as(Frequent_items, 'ngTMatrix'),
                  support = Frequent_items_support)
  
  
  # itemsets class from arules
  Frequent_items_itemsets <- readRDS(system.file("testdata","coerce_types_apriori_output.rds",
                                                 package="ProjectApriori"))
  
  # transactions object from arules
  #Frequent_items_transactions <- as(t(Frequent_items),"transactions")
  
  # data frame
  Frequent_items_dataframe <- as.data.frame(Frequent_items)
  
  # Logical, sparse, non-compressed matrix
  Frequent_items_lgTMatrix <- as(Frequent_items, "TsparseMatrix")
  
  # logical, sparse, compressed matrix
  Frequent_items_lgCMatrix <- as(Frequent_items, "CsparseMatrix")
  
  # pattern, sparse, non-compressed matrix
  Frequent_items_ngTMatrix <- as(Frequent_items, "ngTMatrix")
  
  # pattern, sparse, compressed matrix 
  Frequent_items_ngCMatrix <-as(Frequent_items, "ngCMatrix")
  
  
  # Tests for itemset class
  expect_equal(makeFIMatrix(Frequent_items_itemsets,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_itemsets,
                            support = NULL, dataset = dataset), result)
  
  # Test for transaction class
  # expect_equal(makeFIMatrix(Frequent_items_transactions,
  #                           support = Frequent_items_support), result)
  # expect_equal(makeFIMatrix(Frequent_items_transactions,
  #                           support = NULL, dataset = dataset), result)
  
  # Test for data.frame
  expect_equal(makeFIMatrix(Frequent_items_dataframe,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_dataframe,
                            support = NULL, dataset = dataset), result)
  
  # Test for Logical, sparse, non-compressed matrix
  expect_equal(makeFIMatrix(Frequent_items_lgTMatrix,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_lgTMatrix,
                            support = NULL, dataset = dataset), result)
  
  # Test for pattern, sparse, non-compressed matrix
  expect_equal(makeFIMatrix(Frequent_items_lgCMatrix,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_lgCMatrix,
                            support = NULL, dataset = dataset), result)
  
  # Test for pattern, sparse, non-compressed matrix
  expect_equal(makeFIMatrix(Frequent_items_ngTMatrix,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_ngTMatrix,
                            support = NULL, dataset = dataset), result)
  
  # Test for pattern, sparse, compressed matrix 
  expect_equal(makeFIMatrix(Frequent_items_ngCMatrix,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items_ngCMatrix,
                            support = NULL, dataset = dataset), result)
  
  # Test for simple R matrix 
  expect_equal(makeFIMatrix(Frequent_items,
                            support = Frequent_items_support), result)
  expect_equal(makeFIMatrix(Frequent_items,
                            support = NULL, dataset = dataset), result)
})
