#' Frequent Itemsets base on minimal support
#' This function gives the frequenitemsets given a minimal level of support.
#' @param dataset This should be an incident matrix of the baskets where the rows describe the items and the columns describe the individual bastets. Naming of the items is important
#' @param minsupport This defines the minimal level of the support the resulting itemsets should have.
#' @return The is a list that does store the frequent itemsets as a incidence matrix as well as the resulting support as a vector.
#' @name FrequentItemsets

FrequentItemsets <- function(dataset, minsupport){
  
  # Calculate frequent Itemsets of size 1 #
  L1 <- apply(dataset,1 , mean)
  L1 <- L1[L1 >= minsupport]
  L1_names <- names(L1)
  L1_supp <- as.numeric(L1)
  L1 <- diag(rep(TRUE, length(L1_names)))
  rownames(L1) <- L1_names

  # Calculate all candidates of size 2 
  # How many combinations are there, these do represent the columns in the new matrix
  cols_need <- sum((1:(nrow(L1) - 1)))
  
  # Here the matrix is initialized. It does contain all items/rows from L1 and the amount or columns that are needed.#
  L2 <- matrix(rep(FALSE, nrow(L1) * cols_need), nrow = nrow(L1), dimnames = list(rownames(L1),NULL))

  # This numbers do represent the different columns and are used for the positions of the ones.
  cols <- 0:(cols_need - 1) * nrow(L1)
  
  # Here I calculate first the ones of the first element and then of the secon (per column there are two one, since items)
  pos <- c(rep(1:(nrow(L1) - 1), times = (nrow(L1) - 1):1 ) + cols, unlist(lapply(2:nrow(L1), seq, to = nrow(L1))) + cols)

  # I overwrite the corresponding elements with true #
  L2[pos] <- TRUE
  
  # Check frequency of itemsets of size 2 
  
  # Delete itemset that do not have minimal support 
  
  k <- 3
  while (length(get(paste("L", k - 1, sep = ""))) > 0 ){
    
    #assign(paste("L", k, sep = ""), ...)
    
    #k <- k + 1
    break
  }

  return(L2)
  
}



# ## Get Sample data ##
# data("Groceries")
# Groc <-  makeTansactionMatrix(Groceries)
# 
# a <- FrequentItemsets(Groc, minsupport = 0.08)
# 
# b <- GenCandidates(a)
# c <- GenCandidates(b)
# d <- GenCandidates(c)
# e <- GenCandidates(d)
# f <- GenCandidates(e)
# g <- GenCandidates(f)



## Benchmarking the performance of my generation of candidates of length 2 vs combn 
# 
# L1 <- diag(rep(TRUE, 250))
# L1
# 
# 
# 
# library(microbenchmark)
# microbenchmark({combn(c(as.character(1:250)), 2)}, unit = 'ms')
# 
# microbenchmark({
#   cols_need <- sum((1:(nrow(L1) - 1)))
#   cols_need
#   L2 <- matrix(rep(FALSE, nrow(L1) * cols_need), nrow = nrow(L1))
#   cols <- 0:(cols_need - 1) * nrow(L1)
#   
#   pos <- c(rep(1:(nrow(L1) - 1), times = (nrow(L1) - 1):1 ) + cols, unlist(lapply(2:nrow(L1), seq, to = nrow(L1))) + cols)
#   L2[pos] <- TRUE
#   }, unit = 'ms')
