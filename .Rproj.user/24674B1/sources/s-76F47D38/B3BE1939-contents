context("Test DetRules")


test_that("A simple test for rule Generation of consequent_length one", {
  
  items <- matrix(c(TRUE, TRUE, FALSE, FALSE,
                    TRUE, FALSE, TRUE, FALSE,
                    FALSE, TRUE, TRUE, TRUE,  
                    TRUE, TRUE, TRUE, FALSE),nrow = 4, dimnames = list( c("Bread", "Milk", "Diaper", "Beer")))
  
  lhs <- matrix(c(
    FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
    TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
    FALSE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE
  ), nrow = 4,dimnames = list( c("Bread", "Milk", "Diaper", "Beer")), byrow = TRUE)
  
  rhs <- matrix(c(
    TRUE, FALSE,  TRUE, FALSE, FALSE, FALSE, FALSE,  TRUE,  FALSE, FALSE,
    FALSE,  TRUE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE, FALSE,
    FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE
  ), nrow = 4, dimnames = list( c("Bread", "Milk", "Diaper", "Beer")), byrow = TRUE)
  
  supp <- c(0.8, 0.8, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
  
  res <- list(lhs = lhs, rhs = rhs, support = supp)
  
  expect_equal(DetRules(items, 1, Items_support = c(0.8,0.3,0.4,0.4)), res)
})