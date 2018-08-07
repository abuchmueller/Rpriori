#' Apriori Gen of candidates K-1
#' 
#' Takes in a incident matrix of transaction, where the rows do describe the potential items and each colomn does represent the itemsets.
#' @name GenCandidates
#' @export
#' @param L Incident matrix of the transactions
#' @return The candidate for the next iteration of Apriori

GenCandidates <- function(L){
  
  # Make sure that L does only have unique columns. This is assumed later on. 
  L <- GiveUniqueCol(L)
  
  # K does store the number of items in the current Itemsets. We assume that in L there are only itemsets of a certain length.
  # What is reasonable since this does coincide with the logic of apriori both in the frequent Itemset as well as rule generation.
  
  if (ncol(L) > 0){
    K <- sum(L[,1])
  } else {
    return(L)
  }
  
  # Create output matrix of maximal possible length #
  ncols <- sum(1:(ncol(L)- 1)) 
  nrows <- nrow(L)
  cand <- sparseMatrix(i = c(),
                       j = c(),
                       giveCsparse = FALSE,
                       dim = c(nrows, ncols),
                       dimnames = list(rownames(L), NULL))
  
  
  ##########################
  # Step 1: Joining Itemsets 
  ##########################
  # This iter will count the number of real merges that happened #
  iter <- 1
  
  for (base_col in 1:(ncol(L) - 1)){
    for (ref_col in (base_col + 1):ncol(L)){
      if (sum(L[,base_col] & L[,ref_col]) >= K - 1){
        cand[ ,iter] <- L[,base_col] | L[,ref_col]
        iter <- iter + 1
      } 
    }
  }
  
  
  
  # Cut ouput matrix back on relevant (non only zero) values #
  if (iter > 2){
    cand <- GiveUniqueCol(cand[,1:(iter - 1)])
  } else {
    if (any(apply(cand, 2, sum) == 0)){
      cand <- cand[,1, drop = FALSE]
    } else {
      if (ncol(cand) == 1){
        cand <- cand[,1, drop = FALSE]
      } else {
        cand <- cand[,1:2, drop = FALSE]
      }
      
    }
  }
  
  ##################
  # Step 2: Pruning
  #################
  
  # When I multiply the transpose of the candidate set with the original set I can get a maximum of k since there
  # are only K items in the itemsets from L. Now all Subsets of length K of the candidates have also to be frequent
  # itemsets so that the candidate might be a frequent itemsets. Therefore for each candidate multiplied with the L we have
  # to get K+1 complete matches. A complete match is when the candidate multiplied with on itemset from L with K elements
  # results in the value k.
  
  select <- rowSums(t(cand * 1) %*% (L * 1) == K) == K + 1
  
  return(cand[, select, drop = FALSE])
}

# L1 <- readRDS('testdata/optim_GendCandidates_L1.rds')
# 
# profvis({
#   GenCandidates(L1)
# })
