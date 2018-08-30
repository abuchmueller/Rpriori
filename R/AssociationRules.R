# ------------------------------------------------------------------------------------------------ #
# ------------------------------------ AssociationRules ------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #

#' Calculate Association rules with minimal support and confidence.
#'
#' AssocationRules() computes association rules with minimal support and confidence based on a 
#' set of transactions. Additionally, one can precalculate the frequent item-sets externally and
#' provide them via the FrequentItems parameter. Per default only consequent of length 1 are 
#' calculated. This can be changed with the parameter maxConsquentLength.
#' @name AssociationRules
#' @export
#' @param FrequentItems Precalculated frequent itemsets as an sparse matrix, matrix, data.frame, 
#' FIMatrix or itemsets class object.
#' @param Itemsets Object of class TAMatrix, matrix, sparse matrix, data.frame or transactions that
#' contain the trainsaction for which the rules should be calculated.
#' @param minconfidence Minimal confidence level the rules should have.
#' @param minsupport Minimal support level the rules should have. 
#' @param maxConsequentLength Maximal length of the consequents for the generated rules.
#' @return Object of class Rules containing the calculated rules as well as quality measures.
#' @examples \donttest{
#' # Calculate the Rules with minimal support 0.03 
#' # and confidence 0.4 based on the dataset Groceries
#' Groceries_Rules <- AssociationRules(Itemsets = Groceries, minsupport = 0.03, minconfidence = 0.4)
#' 
#' # print the rules
#' print(Groceries_Rules)
#' 
#' # plot the rules
#' plot(Groceries_Rules)
#' }
AssociationRules <- function(Itemsets, minsupport, minconfidence = 0, FrequentItems,
                             maxConsequentLength = 1) {
  
  # Check input types of FrequentItems and Itemsets and make them FIMatrix and TAMatrix if 
  # neccessary.
  if (class(Itemsets)[1] != "TAMatrix") {
    Itemsets <- makeTAMatrix(Itemsets)
  }
  
  if (!missing(FrequentItems) && class(FrequentItems)[1] != "TAMatrix") {
    FrequentItems <- makeFIMatrix(FrequentItems, NULL, Itemsets)
  }
  
  # If the frequent itemsets are not provided they have to be calculated here.
  if (missing(FrequentItems)) {
    # Calculate the frequent itemsets with minimal support minsupport with the help of the 
    # Rapriori function Frequent Itemsets
    FrequentItems <- FindFrequentItemsets(Itemsets, minsupport = minsupport)
  }
  
  # Test whether the input data sets do have minimal support
  if (!missing(FrequentItems)) {
    if (any(FrequentItems@support < minsupport)) {
      FrequentItems <- select(FrequentItems, NULL, FrequentItems@support >= minsupport)
    }
  }
  
  # At this point I will save the FrequentItems to a variable and will later on assign them to 
  # slot in the output rule object. I need that slot during compution of the rules with conequent
  # length > 1 but they do not represent the Frequent itemsets anymore. Therefore, I will overwrite
  # them in the end with the two frequent itemsets.
  FrequentItems_correct <- FrequentItems
  
  # ---------------------------------------- #
  # Calculate rules with consequent length 1 #
  # ---------------------------------------- #
  
  # Only frequent itemsets of length >1 are relevant for rule mining.
  # Therefore, I will select only these itemsets from the itemset matrix. 
  selection <- colSums(FrequentItems) > 1
  FrequentItems <- select(FrequentItems, NULL, selection)
  
  # Check whether FrequentItems contains Itemsets. Otherwise for this support level now rules could
  # be created.
  if (ncol(FrequentItems) == 0) {
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
  R1 <- select(R1, NULL, rel_its)
    
  # It might happen that some Items are no longer relevant for the rules in the sense that they 
  # do not exist anymore. This case is fulfilled if a row in the rhs and lhs does not have any
  # entry with 1.
  rel_item <- !(rowSums(R1, lhs = TRUE) == 0  & rowSums(R1, lhs = FALSE) == 0)
  R1 <- select(R1,rel_item, NULL)
  
  R1@FrequentItemsets <- select(R1@FrequentItemsets,rel_item,NULL)
  
  if (maxConsequentLength == 1) {
    
    # Output the created rules since only rules with consequent length 1 should be calculated.
    R1@FrequentItemsets <- FrequentItems_correct
    return(R1)
  } else {
    if (maxConsequentLength < 1) {
      stop("Only values >= 1 allowed for maxConsequentLength")
    }
    
    # ---------------------------------------------- #
    # Calculate rules with consequents longer than 1 #
    # ---------------------------------------------- #
    
    k <- 2 
    
    # For the creation of rules of length 2 the itemsets have to have at least three items.
    # I will select these relevant itemsets in the rules object r_cur.
    rel_Items <- which(colSums(R1@FrequentItemsets) > 2)
    
    R_cur <- select(R1,NULL,R1@itemsetID %in% rel_Items)
    
    # Abort the loop if there are not at least two rules coming from the same frequent itemset or 
    # the maxconsequentLength is reached.
    while (any(duplicated(R_cur@itemsetID)) && k <= maxConsequentLength) {
      
      # Determine the rules of length k + 1.
      R_cur <- DetRules_K(R_cur)
      
      # Determine support of the lhs and rhs
      supp_lhs <- DetSupport(R_cur@lhs, Itemsets, FALSE)
      supp_rhs <- DetSupport(R_cur@rhs, Itemsets, TRUE)
      
      # Calculate confidence, lift and leverage.
      R_cur@confidence <- R_cur@support / supp_lhs
      R_cur@lift <- R_cur@support / (supp_lhs * supp_rhs)
      R_cur@leverage <- R_cur@support - (supp_lhs * supp_rhs)

      # Prune Rules out do not have minconf #
      rel_its <- R_cur@confidence >= minconfidence
      R_cur <- select(R_cur,NULL,rel_its)
      
      # Assign the generated rules to the object Rk that is dynamically created based on the value
      # for k.
      assign(paste("R", k, sep = ""), R_cur)
      
      # After I saved the R_cur and can now modify for the next iteration.
      # We can only create rules of length k for itemsets that have at least K + 1 items. 
      # Therefore, I will select only these itemsets from R_curr.
      rel_Items <- which(colSums(R_cur@FrequentItemsets) > k + 1)
      
      R_cur <- select(R_cur,NULL,R_cur@itemsetID %in% rel_Items)
      
      k <- k + 1
    }
    
    # ---------------------- #
    # Output all found rules #
    # ---------------------- #
    
    # Initiallize the list in which the different elements (rhs, lfs, support, confidence) from
    # the iterations 1 to k - 1 will be saved.
    out_list <- list()
    
    # Iterate from 1 to k-1 to collect the outputs from the iterations 1 to k -1 to their lists.
    for (i in 1:(k - 1)) {
      out_list[[i]] <- get(paste("R", i, sep = ""))
    }
    
    # Combine all the Rule objects containing the rules with consequent length 1, ..., k-1 to one
    # Rule object containing all rules using the CombineRules function.
    R <- CombineRules(out_list)
      
    # Assign the correct frequent itemsets
    R@FrequentItemsets <- FrequentItems_correct
    return(R)  
  }
}