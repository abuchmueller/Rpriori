context("Test ExtractRules")

test_that("test with consequents of one", {
  input <- readRDS(system.file("testdata","ExtractRules_R1.rds", package="ProjectApriori"))
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
                                      support = c(1,2,3)))
  
  result <- readRDS(system.file("testdata","ExtractRules_result1.rds", package="ProjectApriori"))
  result$Lift <- rep(-1, length(input@confidence))
  result$Leverage <- rep(-1, length(input@confidence))
  
  expect_equal(ExtractRules(input), result)
})

test_that("test with consequents of two",{
  input <- readRDS(system.file("testdata","ExtractRules_R2.rds", package="ProjectApriori"))
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
                                      support = c(1,2,3)))
  
  result <- readRDS(system.file("testdata","ExtractRules_result2.rds", package="ProjectApriori"))
  result$Lift <- rep(-1, length(input@confidence))
  result$Leverage <- rep(-1, length(input@confidence))
  
  expect_equal(ExtractRules(input), result)
})


