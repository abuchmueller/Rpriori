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

AssociationRules <- function(FrequentItems, Itemsets, minsupport = NULL, minconfidence,  arefrequent = TRUE){
  # # #### MANUAL HEAD #####
  # Itemsets = input_sets_spares
  # minsupport = 0.3
  # minconfidence = 0.4
  # arefrequent = FALSE
  # ###########################################################################

  # If arefrequent is false then the frequent itemsets have to be calculated first. 
  if (! arefrequent){
    FrequentItems <- FrequentItemsets(Itemsets, minsupport = minsupport)
    FrequentItems_support <- FrequentItems$support
    FrequentItems <- FrequentItems$sets
  } else {
    FrequentItems_support <- FrequentItems$support
    FrequentItems <- FrequentItems$sets
  }

  # For rules only frequent itemsets of length > 1 are relevant. Therefore, I will select only these itemsets from the itemset matrix. 
  select <- apply(FrequentItems, 2, sum) > 1
  FrequentItems <- FrequentItems[,select]
  FrequentItems_support <- FrequentItems_support[select]
  
  # Iteration 1#
  R1 <- DetRules_1(FrequentItems, Items_support = FrequentItems_support)
  R1$confidence <- R1$support / DetSupport(R1$lhs, Itemsets)

  # Prune Rules out do not have minconf #
  rel_its <- R1$conf >= minconfidence
  R1$lhs <- R1$lhs[,rel_its, drop = FALSE]
  R1$rhs <- R1$rhs[,rel_its, drop = FALSE]
  R1$support <- R1$support[rel_its, drop = FALSE]
  R1$confidence <- R1$confidence[rel_its, drop = FALSE]
  R1$item_id <- R1$item_id[rel_its, drop = FALSE]
    
  # Delete Items that are no longer relevant for the rules.
  rel_item <- !(apply(R1$lhs, 1, sum) == 0  & apply(R1$rhs, 1, sum) == 0)
  R1$lhs <- R1$lhs[rel_item, ,drop = FALSE]
  R1$rhs <- R1$rhs[rel_item, ,drop = FALSE]
  
  # Not sure whether it is smart to cut the items out of the frequent items. 
  R1$frequentItems <- R1$frequentItems[rel_item,]

  k <- 2 
  while (ncol(get(paste("R", k - 1, sep = ""))$rhs) > 0 && ncol(get(paste("R", k - 1, sep = ""))$lhs) > 0){
    # Iteration K 
    R_cur <- DetRules_K(get(paste("R", k - 1, sep = "")))
    R_cur$confidence <- R_cur$support / DetSupport(R_cur$lhs, Itemsets)
    
    # Prune Rules out do not have minconf #
    rel_its <- R_cur$conf >= minconfidence
    R_cur$lhs <- R_cur$lhs[,rel_its, drop = FALSE]
    R_cur$rhs <- R_cur$rhs[,rel_its, drop = FALSE]
    R_cur$support <- R_cur$support[rel_its, drop = FALSE]
    R_cur$confidence <- R_cur$confidence[rel_its, drop = FALSE]
    R_cur$item_id <- R_cur$item_id[rel_its, drop = FALSE]
    
    # Delete Items that are no longer relevant for the rules.
    rel_item <- !(apply(R_cur$lhs, 1, sum) == 0  & apply(R_cur$rhs, 1, sum) == 0)
    R_cur$lhs <- R_cur$lhs[rel_item, , drop = FALSE]
    R_cur$rhs <- R_cur$rhs[rel_item,, drop = FALSE]
    R_cur$frequentItems <- R_cur$frequentItems[rel_item,, drop = FALSE]
    
    ## Assign Items ##
    assign(paste("R", k, sep = ""), R_cur)
    
    
    k <- k + 1
  }
  
  # Collect all frequent rules in list #
  rhsides <- list()
  lhsides <- list()
  support <- c()
  confidence <- c()
  
  for (i in 1:(k-1)){
    rhsides[[i]] <- get(paste("R", i, sep = ""))$rhs
    lhsides[[i]] <- get(paste("R", i, sep = ""))$lhs
    support <- c(support, get(paste("R", i, sep = ""))$support)
    confidence <- c(confidence,get(paste("R", i, sep = ""))$confidence )
  }
  
  return(list(lhs = CombineCands(lhsides), rhs = CombineCands(rhsides), support = support, confidence = confidence))  
}

