
# Testing set-up based on example from wikipedia #
testmat <- matrix(as.logical(c(1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0)), nrow = 5)
rownames(testmat) <- c('a', 'b', 'c', 'd', 'e')
testmat


AprioriGen <- function(L){
  # Of what size are the current datasets? #
  min_n <- sum(L[,1]) - 1
  
  # Create output matrix of maximal possible length #
  ncols <- sum(1:ncol(L)) - 1
  nrows <- nrow(L)
  out_mat <- matrix(rep(NA, nrows * ncols), nrow = nrows)
  rownames(out_mat) <- rownames(L)
  
  # Step 1
  
  # This iter will count the number of real merges that happened #
  iter <- 1
  
  for (base_col in 1:(ncol(L) - 1)){
    for (ref_col in (base_col + 1):ncol(L)){
      if (sum(L[,base_col] & L[,ref_col]) >= min_n){
        out_mat[ ,iter] <- as.integer(L[,base_col] | L[,ref_col])
        iter <- iter + 1
      } 
    }
  }
  
  # Cut ouput matrix back on relevant (non-NA) values #
  out_mat <- out_mat[,1:(iter - 1)] 

  # Step 2
  
  return(out_mat)
}

AprioriGen(testmat)
sum(1:ncol(testmat))
L <- testmat
L[,1] & L[,2]

rm(AprioriGen)

## to do:
# Why are columns more than once? More efficient solution possible? #
# Go on with algorithm #





    