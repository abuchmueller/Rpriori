#' Calculate Association rules with minimal support and confidence.
#'
#' This function calculates association rules of consequent length >= 1 based on frequent itemsets.
#' If the Frequent itemsets are already calculated than one should provide them in the paramter
#' FrequentItems and set the paramter arefrequent to TRUE (default).
#' @name AssociationRules
#' @export
#' @param FrequentItems Object of class TIMatrix that contains the frequent itemsets with minimal
#'   support. The support used to calculate them must be the same as specified in minsupport.
#' @param Itemsets Object of class TAMatrix that does contain the transactions underlying the rule
#'   Mining.
#' @param minconfidence Minimal confidence of the rules.
#' @param minsupport Minimal support of the rules.
#' @param maxConsequentLength What should be the maximum length of the consequents of the rules?
#' @param arefrequent Are the Frequent itemsets provided?
#' @return list with the rhs and lhs of the rules as sparse incident matrices. Also a vector
#'   containing the support and confidence of the inidivual rules is provided. The i'th element of
#'   the vector does represent the rule that is defined by the i'th column of the lhs and rhs
#'   matrix.

AssociationRules <- function(FrequentItems, Itemsets, minsupport = NULL, minconfidence,
                             arefrequent = TRUE, maxConsequentLength = 1){
  
  
  # Check input types of FrequentItems and Itemsets
  if (class(Itemsets)[1] != "TAMatrix"){
    Itemsets <- makeTAMatrix(Itemsets)
  }
  
  if (!missing(FrequentItems) && class(FrequentItems)[1] != "TAMatrix"){
    FrequentItems <- makeFIMatrix(FrequentItems)
  }
  

  # If the frequent itemsets are not provided they have to be calculated here.
  if (! arefrequent){
    # Calculate the frequent itemsets with minimal support minsupport with the help of the 
    # Project_Apriori function Frequent Itemsets
    FrequentItems <- FindFrequentItemsets(Itemsets, minsupport = minsupport)
  }
  
  # Test whether the input data sets do have minimal support
  if (arefrequent){
    if (any(FrequentItems@support < minsupport)){
      FrequentItems <- FrequentItems[,FrequentItems@support >= minsupport]
    }
  }

  # Only frequent itemsets of length >1 are relevant for rules with consequent length > 1.
  # Therefore, I will select only these itemsets from the itemset matrix. 
  select <- colSums(FrequentItems) > 1
  FrequentItems <- FrequentItems[,select]
  
  # Check whether FrequentItems contains Itemsets. Otherwise for this support level now rules could
  # be created.
  if(ncol(FrequentItems) == 0){
    warning("No rules can be calculated for that minimal support and confidence level. 
            Returns empty Rules object")
    return(new('Rules',
               lhs = FrequentItems@data,
               rhs = FrequentItems@data,
               support = numeric(0),
               confidence = numeric(0),
               lift = numeric(0),
               leverage = numeric(0),
               itemsetID = numeric(0),
               FrequentItemsets = FrequentItems))
  }
  
  # Generate Rules of consequent length 1.
  R1 <- DetRules_1(FrequentItems)
  
  # Determine support of lhs and rhs
  supp_lhs <- DetSupport(R1@lhs, Itemsets, FALSE)
  supp_rhs <- DetSupport(R1@rhs, Itemsets, TRUE)
  
  # Calculate confidence, lift and leverage
  R1@confidence <- R1@support / supp_lhs
  R1@lift <- R1@support / (supp_lhs * supp_rhs)
  R1@leverage <- R1@support - (supp_lhs * supp_rhs)

  # Prune out the candidates that do not have minimal confidence.
  rel_its <- R1@confidence >= minconfidence
  R1 <- R1[,rel_its]

    
  # It might happen that some Items are no longer relevant for the rules in the sense that they 
  # do not exist anymore. This case is fulfilled if a row in the rhs and lhs does not have any
  # entry with 1.
  rel_item <- !(rowSums(R1, lhs = TRUE) == 0  & rowSums(R1, lhs = FALSE) == 0)
  R1 <- R1[rel_item,]
  
  R1@FrequentItemsets <- R1@FrequentItemsets[rel_item,]
  
  if (maxConsequentLength == 1){
    return(R1)
  } else {
    if (maxConsequentLength < 1){
      stop("Only values >= 1 allowed for maxConsequentLength")
    }
    # Find rules of consequent longer than 1.
    k <- 2 
    
    # Abort the loop if the generated candidates from the last step are empty.
    while (ncol(get(paste("R", k - 1, sep = ""))) > 0 && k <= maxConsequentLength){
      
      # Determine the rules of length k + 1.
      R_cur <- DetRules_K(get(paste("R", k - 1, sep = "")))
      
      # Determine support of the lhs and rhs
      supp_lhs <- DetSupport(R_cur@lhs, Itemsets, FALSE)
      supp_rhs <- DetSupport(R_cur@rhs, Itemsets, TRUE)
      
      # Calculate confidence, lift and leverage.
      R_cur@confidence <- R1@support / supp_lhs
      R_cur@lift <- R1@support / (supp_lhs * supp_rhs)
      R_cur@leverage <- R1@support - (supp_lhs * supp_rhs)

      
      # Prune Rules out do not have minconf #
      rel_its <- R_cur@confidence >= minconfidence
      R_cur <- R_cur[,rel_its]
      
      
      # It might happen that some Items are no longer relevant for the rules in the sense that they 
      # do not exist anymore. This case is fulfilled if a row in the rhs and lhs does not have any
      # entry with 1.
      rel_item <- !(rowSums(R1, lhs = TRUE) == 0  & rowSums(R1, lhs = FALSE) == 0)
      
      R_cur <- R_cur[rel_item, ]
      R_cur@FrequentItemsets <- R_cur@FrequentItemsets[rel_item,, drop = FALSE]
      
      # Assign the generated rules to the object Rk that is dynamically created based on the value
      # for k.
      assign(paste("R", k, sep = ""), R_cur)
      
      
      k <- k + 1
    }
    
    # Initiallize the list in which the different elements (rhs, lfs, support, confidence) from
    # the iterations 1 to k - 1 will be saved.
    out_list <- list()
    
    # Iterate from 1 to k-1 to collect the outputs from the iterations 1 to k -1 to their lists.
    for (i in 1:(k-1)){
      out_list[[i]] <- get(paste("R", i, sep = ""))
    }
    
    # Return all the relevant ouputs (rhs, lhs, support and confidence) in a list. Additionally
    # the different rhsides and lhsides have to be compbined to one matrix using the CombineCands
    # function of this package.
    return( CombineRules(out_list))  
  }
}

