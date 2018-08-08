#' Generate Candidate of length k + 1 based on frequent itemsets of length k.
#' 
#' Takes in a incident matrix of transaction, where the rows do describe the potential items
#'  and each colomn does represent the itemsets.
#' @name GenCandidates
#' @export
#' @param L Sparse incident matrix of the candidates.
#' @return The candidates of length k + 1 as a sparse incident matrix. rownames are kept.

GenCandidates <- function(L){
  
  # We assume later on that L does only have unique columns. Here we do that.
  L <- GiveUniqueCol(L)
  
  # K does store the number of items in the current Itemsets. We assume that in L
  # there are only itemsets of a certain length since this does coincide with the 
  #logic of apriori both in the frequent Itemset as well as rule generation.
  
  if (ncol(L) > 0){
    K <- sum(L[,1])
  } else {
    return(L)
  }
  
  # Initiate the output matrix of maximal possible length. We will potentially prune it 
  # at the end
  
  # The maximal number of combinations of the input matrices does equal  the sum of the 
  # sequence of 1 to the number of columns - 1  of L. This holds since we have to compare the 
  # first column to all other columns except the first one, the second one to all other 
  # columns except the first and the second and so on.
  ncols <- sum(1:(ncol(L)- 1)) 
  
  # the output matrix should have as many rows as the input matrix L
  nrows <- nrow(L)
  
  cand <- sparseMatrix(i = c(),
                       j = c(),
                       giveCsparse = FALSE,
                       dim = c(nrows, ncols),
                       dimnames = list(rownames(L), NULL))
  
  
  ############################
  # Step 1: Joining Itemsets #
  ############################
  
  # Here, we will combine all itemset that have at least k - 1 common items.
  iter <- 1
  
  # Here we apply the logic described above.
  for (base_col in 1:(ncol(L) - 1)){
    for (ref_col in (base_col + 1):ncol(L)){
      
      # This condition ensure that the two compared itemsets have at least k - 1 common
      # items. Only if the they do, they are  added to the output matrix.
      if (sum(L[,base_col] & L[,ref_col]) >= K - 1){
        cand[ ,iter] <- L[,base_col] | L[,ref_col]
        
        # We need this counter both to keep track of at which column we have to insert the 
        # new candiate as well as for the pruning later on.
        iter <- iter + 1
      } 
    }
  }
  
  
  # Now we delete all entries that were not assigned to the output matrix.
  # Also we make sure that the generated candidates are unique.
  # This has many branches since we have to handle some exception that may occur.
  if (iter > 2){
    
    # normal case with at least 3 itemsets.
    cand <- GiveUniqueCol(cand[,1:(iter - 1)])
  } else {
    
    # There are fewer than three itemsets but our matrix has empty columns 
    if (any(apply(cand, 2, sum) == 0)){
      cand <- cand[,1, drop = FALSE]
    } else {
      
      # There are fewer than three itemsets and our matrix does not have empty columns.
      # Also it does only have one row
      if (ncol(cand) == 1){
        cand <- cand[,1, drop = FALSE]
      } else {
        
        # The matrix does have at least two rows.
        cand <- cand[,1:2, drop = FALSE]
      }
      
    }
  }
  
  ###################
  # Step 2: Pruning #
  ###################
  
  # In the last step we generated potentially many frequent itemsets for which have to determine 
  # the support later on to make sure that they are frequent. Yet we can exlude some itemsets
  # based on the apriori property which states that all subsets of frequent itemsets have
  # to be frequent. When we find any subset of a candidate that is not frequent than we can
  # exluded that candidate.
  
  # When I multiply the transpose of the candidate set with the original set I can get a 
  # maximum of k matches since there are only K items in the itemsets from L. Now all
  # Subsets of length K of the candidates have also to be frequent  itemsets so that the
  # candidate might be a frequent itemsets. Therefore for each candidate multiplied with the L
  # we have to get K + 1 complete matches. A complete match is when the candidate multiplied
  # with on itemset from L with K elements results in the value k.
  
  # matrix * 1 make the boolean matrix numeric.
  select <- rowSums(t(cand * 1) %*% (L * 1) == K) == K + 1
  
  # Return the candidate matrix and exclude the candidates base on the logiv from above.
  return(cand[, select, drop = FALSE])
}

# L1 <- readRDS('testdata/optim_GendCandidates_L1.rds')
# 
# profvis({
#   GenCandidates(L1)
# })
