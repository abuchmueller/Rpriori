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
  L2_sup <- DetSupport(L2, dataset)
  
  # Delete itemset that do not have minimal support 
  L2 <- L2[,L2_sup >= minsupport, drop = FALSE]
  L2_sup <- L2_sup[L2_sup >= minsupport, drop = FALSE]
  
  # Delete the rows that do not have at least on value #
  L2 <- L2[apply(L2,1,sum) >0,]
  
  k <- 3
  while (ncol(get(paste("L", k - 1, sep = ""))) > 0 ){
    
    # Below here is experimental. not really tested yet.
    # Create new candidates from L(k-1)
    assign(paste("L", k, sep = ""), GenCandidates(get(paste("L", k - 1, sep = ""))))
    
    # Calculate support of candidates 
    assign(paste("L", k, "_sup", sep = ""), DetSupport(get(paste("L", k , sep = "")), dataset))
    
    # Only keep candidates with sufficent minimal support
    assign(paste("L", k, sep = ""), get(paste("L", k , sep = ""))[,get(paste("L", k, "_sup", sep = "")) >= minsupport, drop = FALSE])
    assign(paste("L", k, "_sup", sep = ""), get(paste("L", k, "_sup", sep = ""))[get(paste("L", k, "_sup", sep = "")) >= minsupport, drop = FALSE])
    
    # Delete rows that do no have a single product in them. #
    assign(paste("L", k, sep = ""), get(paste("L", k , sep = ""))[apply(get(paste("L", k , sep = "")),1,sum) >0,])
    
    k <- k + 1
  }
  
  # Collect all frequent itemset in list #
  out_list <- list()
  
  for (i in 1:(k-1)){
    out_list[[i]] <- get(paste("L", i, sep = ""))
  }
  
  # combine list two one ouput. #
  return(CombineCands(out_list))

}




## Get Sample data ##

data("Groceries")
Groc <-  makeTansactionMatrix(Groceries)

cands <- FrequentItemsets(Groc, minsupport = 0.05)
cands[[2]]

DetSupport(GenCandidates(cands), Transaction = Groc)


DetSupport(L2)


matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),ncol = 3, dimnames = list(c('a', 'b', 'c', 'd'),NULL))
matrix(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),ncol = 3, dimnames = list(c('a', 'b', 'c', 'd'),NULL))

a
b <- GenCandidates(a)
c <- GenCandidates(b)
d <- GenCandidates(c)
e <- GenCandidates(d)
f <- GenCandidates(e)
g <- GenCandidates(f)



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
