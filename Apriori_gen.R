rm(list = ls())
# Testing set-up based on example from wikipedia #
testmat <- matrix(as.logical(c(1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0)), nrow = 5)
rownames(testmat) <- c('a', 'b', 'c', 'd', 'e')
testmat


AprioriGen <- function(L){
  # Only take the unique values of L #
  L <- unique(L, MARGIN = 2)
  # Of what size are the current datasets? #
  K <- sum(L[,1])
  
  # Create output matrix of maximal possible length #
  ncols <- sum(1:ncol(L)) - 1
  nrows <- nrow(L)
  cand <- matrix(rep(FALSE, nrows * ncols), nrow = nrows)
  rownames(cand) <- rownames(L)
  
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
  cand <- unique(cand[,1:(iter - 1)], MARGIN = 2)

  # Step 2
  # Counter for the number of relevant datasets #
  iter <- 0
  
  # Create the ouput matrix where I will save the relevant candidates #
  rel_cand <- cand
  rel_cand[] <- FALSE
  
  # Prepare the subset matrix that will always have as many rows as L and k - 1 cols #
  subs <- matrix(rep(FALSE, K * nrows ), ncol = K)
  
  for (col_num in 1:ncol(cand)){

    # Create all k - 1 subset of that set #
    # Rep the current set k times to create all K-1 subsets #
    subs <- matrix(rep(cand[,col_num], each = K + 1), ncol = K + 1, byrow = TRUE)
    
    # Calculate the positions of the elements that I have to set to zero to get k-1 itemsets
    pos <- 0:K  * nrows + which(cand[,col_num])
    
    # Now for each subset set one True values to false to have subsets of number K- 1
    subs[pos] <- FALSE
    
    # Check whether all of them are in L #
    # Explaination for the condition:#
    # cbind the current candidates with all observations from L.
    # Then there should K + 1 duplicates when all subset of the current candidate are in L.
    if(sum(duplicated(cbind(L, subs), MARGIN = 2)) == K + 1) {
      rel_cand[,1] <- cand[,col_num]
      
      iter <- iter + 1
    }
    # Only if all of the candidate is true candiate.
    # Put him in the rel_cand set #
    # make counter one higher. #
    
    
  }
  
  return(rel_cand[,1:iter] )
}

t <- AprioriGen(testmat)

names(t)[t]


rm(AprioriGen)

## to do:
# Why are columns more than once? More efficient solution possible? #
# Go on with algorithm #

### TESTING ###
cur_set <- t[,1]
K <- 3
nrows <- 5
cur_set
subs <- matrix(rep(cur_set, each = K + 1), ncol = K + 1, byrow = TRUE)
subs2 <- subs
subs2[0:K  * nrows + which(cur_set)] <- FALSE
subs    
subs2




