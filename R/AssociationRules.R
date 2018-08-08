#' Calculate Association rules with minimal support and confidence.
#' 
#' This function calculates association rules of consequent length >= 1 based on frequent itemsets.
#' If the Frequent itemsets are already calculated than one should provide them in the paramter 
#' FrequentItems and set the paramter arefrequent to TRUE (default).
#' @name AssociationRules
#' @export
#' @param FrequentItems If the function is provided with the frequent itemsets they should
#'  be inserted here. The structure has to be a sparse incident matrix where the rows do represent
#'  the items and the columns do represent the individual itemsets. Also the paramter arefrequent
#'  should be set to TRUE.
#' @param Itemsets Sparse incident matrix of the transactions where the rows represent the items
#' and the columsn the individual itemsets.
#' @param minconfidence Minimal confidence of the rules.
#' @param minsupport Minimal support of the rules.
#' @param arefrequent Are the Frequent itemsets provided?
#' @return list with the rhs and lhs of the rules as sparse incident matrices. Also a vector 
#' containing the support and confidence of the inidivual rules is provided. The i'th element of 
#' the vector does represent the rule that is defined by the i'th column of the lhs and rhs matrix.

AssociationRules <- function(FrequentItems, Itemsets, minsupport = NULL, minconfidence,
                             arefrequent = TRUE){

  # If the frequent itemsets are not provided they have to be calculated here.
  if (! arefrequent){
    # Calculate the frequent itemsets with minimal support minsupport with the help of the 
    # Project_Apriori function Frequent Itemsets
    FrequentItems <- FrequentItemsets(Itemsets, minsupport = minsupport)
    
    # Get the frequent itemsets and the support out of the result of Frequent itemsets.
    FrequentItems_support <- FrequentItems$support
    FrequentItems <- FrequentItems$sets
  } else {
    
    # If the frequent itemsets are provided extract them here.
    FrequentItems_support <- FrequentItems$support
    FrequentItems <- FrequentItems$sets
  }

  # Only frequent itemsets of length >1 are relevant for rules with consequent length > 1.
  # Therefore, I will select only these itemsets from the itemset matrix. 
  select <- apply(FrequentItems, 2, sum) > 1
  FrequentItems <- FrequentItems[,select]
  FrequentItems_support <- FrequentItems_support[select]
  
  # Generate Rules of consequent length 1.
  R1 <- DetRules_1(FrequentItems, Items_support = FrequentItems_support)
  
  # Calculate confidence for all candidates of rules with consequent length 1.
  R1$confidence <- R1$support / DetSupport(R1$lhs, Itemsets)

  # Prune out the candidates that do not have minimal confidence.
  rel_its <- R1$conf >= minconfidence
  R1$lhs <- R1$lhs[,rel_its, drop = FALSE]
  R1$rhs <- R1$rhs[,rel_its, drop = FALSE]
  R1$support <- R1$support[rel_its, drop = FALSE]
  R1$confidence <- R1$confidence[rel_its, drop = FALSE]
  R1$item_id <- R1$item_id[rel_its, drop = FALSE]
    
  # It might happen that some Items are no longer relevant for the rules in the sense that they 
  # do not exist anymore. This case is fulfilled if a row in the rhs and lhs does not have any
  # entry with 1.
  rel_item <- !(apply(R1$lhs, 1, sum) == 0  & apply(R1$rhs, 1, sum) == 0)
  R1$lhs <- R1$lhs[rel_item, ,drop = FALSE]
  R1$rhs <- R1$rhs[rel_item, ,drop = FALSE]
  R1$frequentItems <- R1$frequentItems[rel_item,]

  # Find rules of consequent longer than 1.
  k <- 2 
  
  # Abort the loop if the generated candidates from the last step are empty.
  while (ncol(get(paste("R", k - 1, sep = ""))$rhs) > 0 &&
         ncol(get(paste("R", k - 1, sep = ""))$lhs) > 0){
    
    # Determine the rules of length k + 1.
    R_cur <- DetRules_K(get(paste("R", k - 1, sep = "")))
    
    # Calculate confidence for newly generated rules and save them in the intermediate 
    # object R_cur.
    R_cur$confidence <- R_cur$support / DetSupport(R_cur$lhs, Itemsets)
    
    # Prune Rules out do not have minconf #
    rel_its <- R_cur$conf >= minconfidence
    R_cur$lhs <- R_cur$lhs[,rel_its, drop = FALSE]
    R_cur$rhs <- R_cur$rhs[,rel_its, drop = FALSE]
    R_cur$support <- R_cur$support[rel_its, drop = FALSE]
    R_cur$confidence <- R_cur$confidence[rel_its, drop = FALSE]
    R_cur$item_id <- R_cur$item_id[rel_its, drop = FALSE]
    
    # It might happen that some Items are no longer relevant for the rules in the sense that they 
    # do not exist anymore. This case is fulfilled if a row in the rhs and lhs does not have any
    # entry with 1.
    rel_item <- !(apply(R_cur$lhs, 1, sum) == 0  & apply(R_cur$rhs, 1, sum) == 0)
    R_cur$lhs <- R_cur$lhs[rel_item, , drop = FALSE]
    R_cur$rhs <- R_cur$rhs[rel_item,, drop = FALSE]
    R_cur$frequentItems <- R_cur$frequentItems[rel_item,, drop = FALSE]
    
    # Assign the generated rules to the object Rk that is dynamically created based on the value
    # for k.
    assign(paste("R", k, sep = ""), R_cur)
    
    
    k <- k + 1
  }
  
  # Initiallize the list in which the different elements (rhs, lfs, support, confidence) from
  # the iterations 1 to k - 1 will be saved.
  rhsides <- list()
  lhsides <- list()
  support <- c()
  confidence <- c()
  
  # Iterate from 1 to k-1 to collect the outputs from the iterations 1 to k -1 to their lists.
  for (i in 1:(k-1)){
    rhsides[[i]] <- get(paste("R", i, sep = ""))$rhs
    lhsides[[i]] <- get(paste("R", i, sep = ""))$lhs
    support <- c(support, get(paste("R", i, sep = ""))$support)
    confidence <- c(confidence,get(paste("R", i, sep = ""))$confidence )
  }
  
  # Return all the relevant ouputs (rhs, lhs, support and confidence) in a list. Additionally
  # the different rhsides and lhsides have to be compbined to one matrix using the CombineCands
  # function of this package.
  return(list(lhs = CombineCands(lhsides), rhs = CombineCands(rhsides), support = support, confidence = confidence))  
}

