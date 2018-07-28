#' Calculate Association rules with minimal confidence and support.
#' 
#' This function takes either frequent Itemsets as input or calculates them by itself and based on these itemsets it estimates the rules with minimal confidence.
#' @name AssociationRules
#' @export
#' @param Itemsets This should either be the transaction matrix, the then paramter arefrequent should be set to FALSE or it should be the frequent itemsets with
#' the paramter arefrequent = TRUE.
#' @param minconfidence This should be the minimal confidence the rules are supposed to have.
#' @param minsupport Only necessary if the Itemset are not the frequent Itemsets. Then it should describe the minimal support of the rules.
#' @param arefrequent Are the input Itemsets the frequent Itemsets ?
#' @return Incident matrix of the association rules with minimal support and confidence.

AssociationRules <- function(FrequentItemsets, Itemsets ,minconfidence, minsupport = NULL, arefrequent = TRUE){
  
  ###### MANUAL HEAD #####
  # Itemsets <- items
  # minconfidence = 0.5
  # minsupport = 0.01
  # arefrequent = FALSE
  #######################
  
  
  # If arefrequent is false then the frequent itemsets have to be calculated first. 
  if (! arefrequent){
    FrequentItemsets <- FrequentItemsets(Itemsets, minsupport = minsupport)
    FrequentItemsets_support <- FrequentItemsets$support
    FrequentItemsets <- FrequentItemsets$sets
  }
  
  # For rules only frequent itemsets of length > 1 are relevant. Therefore, I will select only these itemsets from the itemset matrix. 
  select <- apply(FrequentItemsets, 2, sum) > 1
  FrequentItemsets <- FrequentItemsets[,select]
  FrequentItemsets_support <- FrequentItemsets_support[select]
  
  # Iteration 1#
  R1 <- DetRules(FrequentItemsets, len = 1, Items_support = FrequentItemsets_support)
  R1_supp <- R1$support
  R1$support <- NULL
  R1_conf <- R1_supp / DetSupport(R1$lhs, Itemsets)

  # Prune Rules out do not have minconf #
  rel_its <- R1_conf >= minconfidence
  R1$lhs <- R1$lhs[,rel_its, drop = FALSE]
  R1$rhs <- R1$rhs[,rel_its, drop = FALSE]
  R1_supp <- R1_supp[rel_its, drop = FALSE]
  R1_conf <- R1_conf[rel_its, drop = FALSE]
    
  # Delete Items that are no longer relevant for the rules.
  rel_item <- !xor(apply(R1$lhs, 1, sum) == 0, apply(R1$rhs, 1, sum) == 0)
  R1$lhs <- R1$lhs[rel_item,]
  R1$rhs <- R1$rhs[rel_item,]
  
  # Iteration 2 #

  return(Itemsets)  
}


### EXAMPLE ###
#rm(list = ls())
# items <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
#          TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
#          FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
#          TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
#          TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6, dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))
# 
# 
# rules <- AssociationRules(items, minconfidence = 0.5, minsupport = 0.01, arefrequent = FALSE)
# 


# 
# ### EXAMPLE FOR 
# 
# 
# 
# 
# # comparing
# res_me <- FrequentItemsets(items, 0.01)
# res_me <- GetSets(res_me)
# res_arues <-  inspect(apriori(t(items), parameter = list(support = 0.01,  target="frequent itemsets")))
# 
# 
# # print ordered result from apriori and me
# res_arues <- res_arues[order(res_arues$support, decreasing = TRUE),]
# res_me <- res_me[order(res_me$Support, decreasing = TRUE),]
# res_arues
# res_me



