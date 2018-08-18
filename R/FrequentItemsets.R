#' Calculate frequent itemset with minimal support
#' 
#' This function calculate the frequent itemset with a minimal support. 
#' @name FindFrequentItemsets
#' @export
#' @param dataset This should be an sparse incident matrix of the baskets 
#' where the rows describe the items and the columns describe the individual bastets.
#'  Naming of the items is important.
#' @param minsupport This defines the minimal level of the support the resulting itemsets should have.
#' @return The is a list that does store the frequent itemsets as a sparse incidence matrix
#'  as well as the resulting support as a vector.


FindFrequentItemsets <- function(dataset, minsupport){
  
  ##################################
  # Calculate candidates of size 1 #
  ##################################
  
  # The row means do represent the support of a single items since it it the mean of the 
  # occurence of the items in all itemsets
  L1 <- rowMeans(dataset@data)
  
  # Only select the itemsets that have minimal support.
  L1 <- L1[L1 >= minsupport]
  
  # Create a vector the vector containing the names of the items based on the rownames of 
  # the items in the dataset.
  L1_names <- names(L1)
  
  # Create object of class FIMatrix for the frequent itemsets of length 1
  L1 <- new("FIMatrix", 
            data = sparseMatrix(i = 1:length(L1_names),
                                j = 1:length(L1_names),
                                giveCsparse = FALSE,
                                dim = c(length(L1_names), length(L1_names)),
                                dimnames = list(L1_names, NULL)),
            support = as.numeric(L1))

  
  ####################################
  # Calculate Candidates of size > 1 #
  ####################################
  k <- 2
  
  # This loop will create candidates of length k + 1 until the matrix generated 
  # in the last step is empty or has only one entry that is when no new 
  # frequent itemsets of length k _ 1 can be created.
  # The assign and get operators do make the following code a bit more complicated
  # but it is not possible to know in advance what the maximum length of Itemsets 
  # will be.
  while (ncol(get(paste("L", k - 1, sep = ""))@data) > 1 ){

    # Create new candidates from of length k  based on the generated Candidates from the 
    # last step.
    L_temp =  GenCandidates(get(paste("L", k - 1, sep = ""))@data)
    
    # Calculate the support of the newly created Candidates
    L_temp_support =  DetSupport(L_temp, dataset, same_item_num = TRUE)
    
    # Make L_temp object of class TIMatrix so that we can subset both the matrix
    # and the support at the same time.
    L_temp <- new("FIMatrix",
                  data = L_temp,
                  support = L_temp_support)
    
    # Prune the candidates that do not have minimal support.
    L_temp <- L_temp[,L_temp@support >= minsupport]
    
    # It may happen during the generation of Candidates that a certain Item does
    # not occur anymore and does not have any TRUE values in its respective row.
    # Since we do not need this item any longer I delete it here.
    L_temp <- L_temp[rowSums(L_temp@data) > 0,]
    
    # Create new object of class FIMatrix the contains the frequent itemset of length k
    assign(paste("L", k, sep = ""), L_temp)
    
    k <- k + 1
  }
  
  # Here, all generated FIMatrices are collected in a list to combine them.
  
  # Initiallize that list.
  out_list <- list()
  
  # Iterate from i to K-1, the maximum number of successfully generated candidates, and append
  # them to the list and their support to the vector.
  for (i in 1:(k-1)){
    out_list[[i]] <- get(paste("L", i, sep = ""))
  }
  

  # Give back the results as a list containing the Candidates as a sparse incident matrix
  # and their support as a vector.
  # Rely on the CombineCands fuction to combine the different generated Candidates matrices.
  return(CombineFIMatrix(out_list))
}
