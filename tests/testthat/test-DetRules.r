context("Test DetRules 1 and k")


test_that("A simple test for rule Generation of consequent_length one", {
  
  FrequentItems <- Rpriori:::DetRules_1_FrequentItemets
  
  FrequentItems <- new("FIMatrix",
                       data = as(FrequentItems, "ngTMatrix"),
                       support = Rpriori:::DetRules_1_FrequentItems_support)
  
  result <- Rpriori:::DetRules_1_R1
  
  result <- new("Rules",
                lhs = as(result$lhs, "ngTMatrix"),
                rhs = as(result$rhs, "ngTMatrix"),
                support = result$support,
                confidence = rep(0, length(result$support)),
                lift = rep(0, length(result$support)),
                leverage = rep(0, length(result$support)),
                itemsetID = result$item_id,
                FrequentItemsets = FrequentItems)
  
  expect_equal(DetRules_1(FrequentItems) , result)
})

  
test_that("DetRules_K: Generate only one rule of length 2 from rules of length 1", {
  
  # Example for rules of length 2 where only one rules is created (Special Case)
  # Create input set #
  input <- Rpriori:::test_Detrules1 
  
  input <- new("Rules",
               lhs = as(input$lhs, "ngTMatrix"),
               rhs = as(input$rhs, "ngTMatrix"),
               support = input$support,
               confidence = rep(0, length(input$support)),
               lift = rep(0, length(input$support)),
               leverage = rep(0, length(input$support)),
               itemsetID = input$item_id,
               FrequentItemsets = new("FIMatrix",
                                      data = as(input$frequentItems, "ngTMatrix"),
                                      support = rep(0, ncol(input$frequentItems))))

  
  # Create output set #
  result <- Rpriori:::test_Detrules2
  
  result <- new("Rules",
                lhs = as(result$lhs, "ngTMatrix"),
                rhs = as(result$rhs, "ngTMatrix"),
                support = result$support,
                confidence = rep(0, length(result$support)),
                lift = rep(0, length(result$support)),
                leverage = rep(0, length(result$support)),
                itemsetID = result$item_id,
                FrequentItemsets = input@FrequentItemsets)

  expect_equal(DetRules_K(input), result)
})
