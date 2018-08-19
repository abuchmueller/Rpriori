context("Test DetRules 1 and k")


test_that("A simple test for rule Generation of consequent_length one", {
  
  FrequentItems <- readRDS(system.file("testdata","DetRules_1_FrequentItemets.rds", package="ProjectApriori"))
  
  FrequentItems <- new("FIMatrix",
                       data = as(FrequentItems, "ngTMatrix"),
                       support = readRDS(system.file("testdata",
                                                     "DetRules_1_FrequentItems_support.rds",
                                                     package="ProjectApriori")))
  
  result <- readRDS(system.file("testdata","DetRules_1_R1.rds", package="ProjectApriori"))
  
  result <- new("Rules",
                lhs = as(result$lhs, "ngTMatrix"),
                rhs = as(result$rhs, "ngTMatrix"),
                support = result$support,
                confidence = rep(-1, length(result$support)),
                lift = rep(-1, length(result$support)),
                leverage = rep(-1, length(result$support)),
                itemsetID = result$item_id,
                FrequentItemsets = as(result$frequentItems, "ngTMatrix"))
  
  expect_equal(DetRules_1(FrequentItems) , result)
})

  
test_that("DetRules_K: Generate only one rule of length 2 from rules of length 1", {
  
  # Example for rules of length 2 where only one rules is created (Special Case)
  # Create input set #
  input <- readRDS(system.file("testdata","test_Detrules1.rds", package="ProjectApriori"))
  
  input <- new("Rules",
               lhs = as(input$lhs, "ngTMatrix"),
               rhs = as(input$rhs, "ngTMatrix"),
               support = input$support,
               confidence = rep(-1, length(input$support)),
               lift = rep(-1, length(input$support)),
               leverage = rep(-1, length(input$support)),
               itemsetID = input$item_id,
               FrequentItemsets = as(input$frequentItems, "ngTMatrix"))

  
  # Create output set #
  result <- readRDS(system.file("testdata","test_Detrules2.rds",package="ProjectApriori"))
  
  result <- new("Rules",
                lhs = as(result$lhs, "ngTMatrix"),
                rhs = as(result$rhs, "ngTMatrix"),
                support = result$support,
                confidence = rep(-1, length(result$support)),
                lift = rep(-1, length(result$support)),
                leverage = rep(-1, length(result$support)),
                itemsetID = result$item_id,
                FrequentItemsets = as(result$frequentItems, "ngTMatrix"))

  expect_equal(DetRules_K(input), result)
})
