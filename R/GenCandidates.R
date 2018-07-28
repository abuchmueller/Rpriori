#' Apriori Gen of candidates K-1
#' 
#' Takes in a incident matrix of transaction, where the rows do describe the potential items and each colomn does represent the itemsets.
#' @name GenCandidates
#' @param L Incident matrix of the transactions
#' @return The candidate for the next iteration of Apriori

GenCandidates <- function(L){
  
  # Only take the unique values of L #
  L <- unique(L, MARGIN = 2)
  # Of what size are the current datasets? #
  if (ncol(L) > 0){
  K <- sum(L[,1])
  } else {
    return(L)
  }
  
  # Create output matrix of maximal possible length #
  ncols <- sum(1:ncol(L)) - 1
  nrows <- nrow(L)
  cand <- matrix(rep(FALSE, nrows * ncols), nrow = nrows, dimnames = list(rownames(L), NULL))
  
  # Step 1
  
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
    cand <- unique(cand[,1:(iter - 1)], MARGIN = 2)
  } else {
    if (any(apply(cand, 2, sum) == 0)){
      cand <- cand[,1, drop = FALSE]
    } else {
      cand <- cand[,1:2, drop = FALSE]
    }
  }


  # Step 2
  # Counter for the number of relevant datasets #
  iter <- 1
  
  # Create the ouput matrix where I will save the relevant candidates #
  rel_cand <- cand
  rel_cand[] <- FALSE
  
  # Prepare the subset matrix that will always have as many rows as L and k - 1 cols #
  #subs <- matrix(rep(FALSE, K * nrows ), ncol = K)
  
  for (col_num in 1:ncol(cand)){

    # Create all k - 1 subset of that set #
    # Rep the current set k times to create all K-1 subsets #
    subs <- matrix(rep(cand[,col_num], each = K + 1), ncol = K + 1, byrow = TRUE)
    
    # Calculate the positions of the elements that I have to set to zero to get k-1 itemsets
    
    # Check whether all positions are TRUE, if the case use other logic
    if (! all(subs)){
      pos <- 0:K  * nrows + which(cand[,col_num])
    } else {
      pos <- 0:K  * nrows + 1:4
    }
    
    
    # Now for each subset set one True values to false to have subsets of number K- 1
    subs[pos] <- FALSE
    
    # Check whether all of them are in L #
    # Explaination for the condition:#
    # cbind the current candidates with all observations from L.
    # Then there should K + 1 duplicates when all subset of the current candidate are in L.
    if(sum(duplicated(cbind(L, subs), MARGIN = 2)) == K + 1) {
      rel_cand[,iter] <- cand[,col_num]
      
      iter <- iter + 1
    }
    # Only if all of the candidate is true candiate.
    # Put him in the rel_cand set #
    # make counter one higher. #
    
    
  }

  if(iter > 1){
  return(rel_cand[,1:(iter - 1), drop = FALSE])
  } else {
    return(rel_cand[,0, drop = FALSE])
  }
}










