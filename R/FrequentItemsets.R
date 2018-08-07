#' Frequent Itemsets base on minimal support
#' 
#' This function gives the frequenitemsets given a minimal level of support.
#' @name FrequentItemsets
#' @export
#' @param dataset This should be an incident matrix of the baskets where the rows describe the items and the columns describe the individual bastets. Naming of the items is important
#' @param minsupport This defines the minimal level of the support the resulting itemsets should have.
#' @return The is a list that does store the frequent itemsets as a incidence matrix as well as the resulting support as a vector.


FrequentItemsets <- function(dataset, minsupport){
  
  ######################################################
  # Manual Insertion ... pls delete
  # dataset = input_sets_spares
  # minsupport = 0.3
  ########################################################
  
  # Calculate frequent Itemsets of size 1 #
  L1 <- apply(dataset,1 , mean)
  L1 <- L1[L1 >= minsupport]
  L1_names <- names(L1)
  L1_sup <- as.numeric(L1)
  L1 <- sparseMatrix(i = 1:length(L1_names),
                          j = 1:length(L1_names),
                          giveCsparse = FALSE,
                          dim = c(length(L1_names), length(L1_names)),
                          dimnames = list(L1_names, NULL))
  
  # Calculate all frequent Itemsets with size > 1
  k <- 2
  while (ncol(get(paste("L", k - 1, sep = ""))) > 1 ){

    # Create new candidates from L(k-1)
    assign(paste("L", k, sep = ""), GenCandidates(get(paste("L", k - 1, sep = ""))))
    
    # Calculate support of candidates 
    assign(paste("L", k, "_sup", sep = ""), DetSupport(get(paste("L", k , sep = "")), dataset))
    
    # Only keep candidates with sufficent minimal support
    assign(paste("L", k, sep = ""), get(paste("L", k , sep = ""))[,get(paste("L", k, "_sup", sep = "")) >= minsupport, drop = FALSE])
    assign(paste("L", k, "_sup", sep = ""), get(paste("L", k, "_sup", sep = ""))[get(paste("L", k, "_sup", sep = "")) >= minsupport, drop = FALSE])
    
    # Delete rows that do no have a single product in them. #
    assign(paste("L", k, sep = ""), get(paste("L", k , sep = ""))[apply(get(paste("L", k , sep = "")),1,sum) >0,,drop = FALSE])

    k <- k + 1
  }
  
  # Collect all frequent itemset in list #
  out_list <- list()
  support <- c()
  
  for (i in 1:(k-1)){
    out_list[[i]] <- get(paste("L", i, sep = ""))
    support <- c(support, get(paste("L", i, "_sup", sep = "")))
  }
  

  # combine list two one ouput. #
  return(list(sets = CombineCands(out_list), support = support))
}
