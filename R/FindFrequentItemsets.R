# ------------------------------------------------------------------------------------------------ #
# ----------------------------------- FindFrequentItemsets --------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Calculate frequent itemset with minimal support
#' 
#' This function calculates the frequent itemset with a minimal support value. 
#' @name FindFrequentItemsets
#' @export
#' @param dataset Transactions in an object of class TAMatrix, matrix, sparse matrix, data.frame or
#' transactions.
#' @param minsupport Minimal support value of the frequent itemsets that are calculated.
#' @return Object of class FIMatrix containing the frequent items.
#' @examples \donttest{
#' # Calculate the frequent itemsets with minimal support 0.03 
#' # and confidence 0.4 based on the dataset Groceries
#' Groceries_Fitems <- FindFrequentItemsets(Itemsets = Groceries, minsupport = 0.03)
#' 
#' # print the frequent itemsets
#' print(Groceries_Fitems)
#' 
#' # plot the frequent itemsets
#' plot(Groceries_Fitems)
#' }
FindFrequentItemsets <- function(dataset, minsupport){
  
  # Ensure that dataset is a TAMatrix.
  dataset <- makeTAMatrix(dataset)
  
  # ------------------------------------- #
  # Calculate frequent itemsets of size 1 #
  # ------------------------------------- #
  
  # The row means do represent the support of a single items since it it the mean of the 
  # occurence of the items in all itemsets
  L1 <- rowMeans(dataset@data)
  
  # Only select the itemsets that have minimal support.
  L1 <- L1[L1 >= minsupport]
  
  # Create a vector the vector containing the names of the items based on the rownames of 
  # the items in the dataset.
  L1_names <- names(L1)
  
  # prepare creation of a sparse matrix.
  len_names <- length(L1_names)
  
  # The initial matrix should be diagonal matrix since it contains only itemsets of length 1. 
  # For a sparse matrix we have to give explicitly the rows and columns of the items that are TRUE
  # Hence for  a diagonal matrix the respective row and column numbers must be 1, 2, ... , n.
  vec_names <- 1:len_names
  
  # If L1 is empty return empty FIMatrix #
  if (len_names == 0) {
    warning(paste("No frequent itemsets found for the support", minsupport, "Returning empty FIMatrix object."))
    return(new("FIMatrix", 
              data = sparseMatrix(i = numeric(0),
                                  j = numeric(0),
                                  giveCsparse = FALSE,
                                  dims = c(0, 0)),
                                  support = numeric(0)))
  }
  
  # Create object of class FIMatrix for the frequent itemsets of length 1
  L1 <- new("FIMatrix", 
            data = sparseMatrix(i = vec_names,
                                j = vec_names,
                                giveCsparse = FALSE,
                                dims = c(len_names, len_names),
                                dimnames = list(L1_names, NULL)),
            support = as.numeric(L1))

  # --------------------------------------- #
  # Calculate frequent Itemsets of size > 1 #
  # --------------------------------------- #
  k <- 2
  
  # This loop will create frequent itemsets of length k + 1 until the matrix generated 
  # in the last step is empty or has only one entry that is when no new 
  # frequent itemsets of length k + 1 can be created.
  # The assign and get operators do make the following code a bit more complicated.
  # At the end of each step we save the created frequent itemsets as L_k.
  while (ncol(get(paste("L", k - 1, sep = ""))@data) > 1 ) {

    # Create new candidates from of length k  based on the generated frequent itemssets from the 
    # last step.
    L_temp =  GenCandidates(get(paste("L", k - 1, sep = ""))@data)
    
    # Calculate the support of the newly created Candidates.
    L_temp_support =  DetSupport(L_temp, dataset, same_item_num = TRUE)
    
    # Make L_temp object of class TIMatrix so that we can subset both the matrix
    # and the support at the same time.
    L_temp <- new("FIMatrix",
                  data = L_temp,
                  support = L_temp_support)
    
    # Prune the candidates that do not have minimal support.
    L_temp <- select(L_temp, NULL, L_temp@support >= minsupport)
    
    # It may happen during the generation of Candidates that certain Items do
    # not occur anymore and do not have any TRUE values in its respective row.
    # Since we do not need this item any longer I delete it here.
    L_temp <- select(L_temp, rowSums(L_temp@data) > 0, NULL)
    
    # Create new object of class FIMatrix the contains the frequent itemset of length k
    assign(paste("L", k, sep = ""), L_temp)
    
    k <- k + 1
  }
  
  # Here, all generated FIMatrices are collected in a list to combine them.
  
  # Initiallize that list.
  out_list <- list()
  
  # Iterate from i to K-1, the maximum number of successfully generated candidates, and append
  # them to the list and their support to the vector.
  for (i in 1:(k - 1)) {
    out_list[[i]] <- get(paste("L", i, sep = ""))
  }
  
  # Return all frequent itemsets that could be found in one big FIMatrix.
  # Rely on the CombineCands fuction to combine the different generated FIMatrices.
  return(CombineFIMatrix(out_list))
}