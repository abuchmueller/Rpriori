#' Apriori Gen of candidates K-1
#' 
#' Takes in a incident matrix of transaction, where the rows do describe the potential items and each colomn does represent the itemsets.
#' @name GenCandidates
#' @export
#' @param L Incident matrix of the transactions
#' @return The candidate for the next iteration of Apriori

GenCandidates <- function(L){
  ## Manual Header ##
  # L <- rules$rhs[,rules$item_id == unique_ids[f_it], drop = FALSE]
  ####################
  
  # Create numeric, sparse matrix from L #
  L_num <- sparseMatrix(i = L@i,
            j = L@j,
            x = rep(1, length(L@i)),
            giveCsparse = FALSE,
            index1 = FALSE,
            dim = c(nrow(L), ncol(L)),
            dimnames = list(rownames(L), NULL))
  
  L <- GiveUniqueCol(L)
  
  # Of what size are the current datasets? #
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
  
  
  # Counter for the number of relevant datasets #
  iter <- 1
  
  # Create the ouput matrix where I will save the relevant candidates #
  rel_cand <- cand
  rel_cand[] <- FALSE
  
  
  for (col_num in 1:ncol(cand)){
    
    # Create all k - 1 subset of that set #
    # Rep the current set k times to create all K-1 subsets #
    true_rows <- cand[,col_num, drop = FALSE]@i + 1
    true_cols <- rep(1:(K + 1), each = length(true_rows))

    true_rows <- rep(true_rows, K + 1)
    
    subs <- sparseMatrix(i = true_rows,
                         j = true_cols,
                         giveCsparse = FALSE,
                         x = rep(1, length(true_rows)),
                         dim = c(nrow(cand), K + 1),
                         dimnames = list(rownames(cand), NULL))
    # Calculate the positions of the elements that I have to set to zero to get k-1 itemsets
    
    # Check whether all positions are TRUE, if the case use other logic
    if (! all(subs)){
      pos <- 0:K  * nrows + which(cand[,col_num])
    } else {
      ####################################################################
      ### WHATS THAT's ALL ABOUT ??? 1:4 in every case? SURE? INVESTIGATE!
      ####################################################################
      pos <- 0:K  * nrows + 1:4
    }
    
    
    # Now for each subset set one True values to false to have subsets of number K- 1
    subs[pos] <- FALSE
    
    # Here we have to check whether all candidates in subs are also in the frequent
    # itemsets from last iteration (L). If true then add them to the relevant candidate set else don't 
    if( all(rowSums(t(subs) %*% L >= K)) >= 1){
      rel_cand[,iter] <- cand[, col_num]
      iter <- iter + 1
    }
    
    
  }

  if(iter > 1){
  return(rel_cand[,1:(iter - 1), drop = FALSE])
  } else {
    return(rel_cand[,0, drop = FALSE])
  }
}




