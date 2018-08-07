#' Determine the support of a matrix of candidate set within the transactions
#' 
#' @param cand This should represent the matrix of  candidates for which the support has to be checked.
#' It has to be an Incidence matrix where the rows do describe the different items.
#' Each columns should represent one candidate.
#' Also it has to have rownames that are a subset of the rownames of Transaction.
#' @param Transaction This should be all the transaction from which the occurence (support) of the candidate should be checked. It has to be a incident matrix 
#' with named rows that do represent the different items.
#' @import Matrix
#' @name DetSupport
#' @return The support of the candidates wihtin the transactions as a vector of floating-point value.

DetSupport <- function(cand, Transaction){
  
  if (ncol(cand) == 0 ){
    return(c(1)[0])
  }  
  
  # Make cand matrix to integer sparse matrix #
  cand_filled <- sparseMatrix(i = c() , j = c(),
                      dims = c(nrow(Transaction), ncol(cand)),
                      index1 = FALSE,
                      giveCsparse = TRUE,
                      dimnames= list(row.names(Transaction),NULL))
  
  
  cand_filled[rownames(Transaction) %in% rownames(cand),] <- cand
    
  cand <- t(cand_filled * 1)
  
  # Make Transactions matrix to integer sparse matrix #
  Transaction <- sparseMatrix(i = Transaction@i , j = Transaction@j,
                       x = rep(1, length(Transaction@i)),
                       dims = c(nrow(Transaction), ncol(Transaction)),
                       index1 = FALSE,
                       giveCsparse = TRUE,
                       dimnames= list(row.names(Transaction),NULL))
  
  frequs <- cand %*% Transaction
  itemNum <- rowSums(cand)
  
  comp <- frequs >= itemNum
  
  support <- rowSums(comp)
  
  return(support / ncol(Transaction))
}

# L1 <- readRDS('testdata/optim_GendCandidates_L1.rds')
# data("Groceries")
# groc_trans <- makeTansactionMatrix(Groceries)
# 
# 
# L2 <- GenCandidates(L1)
# 
# profvis({
#   sup <- DetSupport(L2, groc_trans )
# })

# 
# profvis(
#   {
#      comp <- frequs - itemNum
#      num_rows <- nrow(comp)
#     t <- apply(comp, 2, function(x) return(num_rows - nnzero(comp)))
#   }
# )
# 


