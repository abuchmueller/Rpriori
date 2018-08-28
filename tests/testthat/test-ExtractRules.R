context("Test ExtractRules")

test_that("test with consequents of one", {
  input <- ProjectApriori:::ExtractRules_R1
  input <- new('Rules',
               lhs = as(input$lhs, 'ngTMatrix' ),
               rhs = as(input$rhs, 'ngTMatrix'),
               support = input$support,
               confidence =  input$confidence,
               lift = rep(-1, length(input$support)),
               leverage = rep(-1, length(input$support)),
               itemsetID = rep(-1, length(input$support)),
               FrequentItemsets = new("FIMatrix",
                                      data = as(input$frequentItems,'ngTMatrix'),
                                      support = rep(0, ncol(input$frequentItems))))
  
  result <-  ProjectApriori:::ExtractRules_result1
  result$Lift <- rep(-1, length(input@confidence))
  result$Leverage <- rep(-1, length(input@confidence))
  
  expect_equal(ExtractRules(input), result)
})

test_that("test with consequents of two",{
  input <- ProjectApriori:::ExtractRules_R2
  input <- new('Rules',
               lhs = as(input$lhs, 'ngTMatrix' ),
               rhs = as(input$rhs, 'ngTMatrix'),
               support = input$support,
               confidence =  input$confidence,
               lift = rep(-1, length(input$support)),
               leverage = rep(-1, length(input$support)),
               itemsetID = rep(-1, length(input$support)),
               FrequentItemsets = new("FIMatrix",
                                      data = as(input$frequentItems, "ngTMatrix"),
                                      support = rep(0, ncol(input$frequentItems))))
  
  result <- ProjectApriori:::ExtractRules_result2
  result$Lift <- rep(-1, length(input@confidence))
  result$Leverage <- rep(-1, length(input@confidence))
  
  expect_equal(ExtractRules(input), result)
})


