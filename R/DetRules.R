#' Determine all possible association rules of length len for given frequent transaction.
#' 
#' This function take the frequent transactions as input and generates rules of length len. It uses the Apriori-gen for more efficient generation
#' of rule candidates.
#' @name DetRules
#' @param Items Frequent Itemsets of different length, but at least two.
#' @param len length the resulting rules in the sense that on the right side there habe to be at least len items

DetRules <- function(Items, len, Items_support){
  
  # ### Manual Header #####
  # Items <- FrequentItemsets
  # len <- 1
  # Items_support <- FrequentItemsets_support
  # #######################
  
  # Select only the Items that are relevant. For a rule of length len there have to be at least len + 1 items in the itemsets #
  select <- apply(Items, 2, sum) >= len + 1
  Items <- Items[, select]
  Items_support <- Items_support[select]
  

  # Here I save the number of items for each input set.
  item_n <- apply(Items, 2, sum) 
  
  # Here I save the positions at which the elements are not FALSE #
  pos_items <- apply(Items, 2, which)
  
  # If len is 1 apriori gen cannot be used. #
  if (len == 1){
    
    # Initialize ouput matrices 
    ncols <- sum(item_n)
    lhs <- matrix(rep(FALSE, ncols * nrow(Items)), nrow = nrow(Items), dimnames = list(rownames(Items)))
    rhs <- matrix(rep(FALSE, ncols * nrow(Items)), nrow = nrow(Items), dimnames = list(rownames(Items)))
    
    # for the left hand side reproduce the positions at which the matrix should be one. #
    cols <- 0:(ncols - 1) * nrow(Items)
    pos <- rep(cols, times = rep(item_n, times = item_n)) + unlist(rep(pos_items, times = item_n))
    lhs[pos] <- TRUE
    
    # Also only in the first iteration also give the initial support work since it did not change.
    Items_support <- rep(Items_support, times = item_n)
    
    # Now I will calcualte the positions at which I should set the value to False to create a subset of k - 1
    
    # lhs 
    pos <- 0:(ncols -1) * nrow(Items) + unlist(pos_items)
    lhs[pos] <- FALSE
    
    # rhs 
    # here the element I had to set to False have to be set to true.
    rhs[pos] <- TRUE
    
    return(list(lhs = lhs, rhs = rhs, support = Items_support))
  }
}

# 
# # Example for len 1 #
# items <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
#                   TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
#                   FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
#                   TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6, dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))
# 
# 
# rules <- DetRules(items, 1)
# ExtractRules(rules)
# 
# # Example 2 
# 
# items <- matrix(as.logical(c(1,1,1,0,0,0,
#                              1,0,0,1,0,0,
#                              0,0,0,1,1,0,
#                              1,1,0,1,0,0,
#                              1,1,1,1,0,1,
#                              0,1,1,0,0,1,
#                              0,1,1,0,0,1 )),nrow = 6, dimnames = list( c("Beef", "Chicken", "Milk", "Cheese", "Boots", "Clothes")))
# 
# res_apriori <- apriori(t(items), parameter = list(support = 0.3, confidence = 0.8) )
# inspect(res_apriori)
# 
# rules <- DetRules(items, 1)
# ExtractRules(rules)

# Example for len 2 #
# Example for len 3 #