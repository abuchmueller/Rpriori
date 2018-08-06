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
                      giveCsparse = FALSE,
                      dimnames= list(row.names(Transaction),NULL))
  
  
  cand_filled[rownames(Transaction) %in% rownames(cand),] <- cand
    
  cand <- sparseMatrix(i = cand_filled@j , j = cand_filled@i,
                       x = rep(1, length(cand_filled@i)),
                       dims = c( ncol(cand_filled), nrow(cand_filled)),
                       index1 = FALSE,
                       dimnames= list(NULL,row.names(cand_filled)))
  
  # Make Transactions matrix to integer sparse matrix #
  Transaction <- sparseMatrix(i = Transaction@i , j = Transaction@j,
                       x = rep(1, length(Transaction@i)),
                       dims = c(nrow(Transaction), ncol(Transaction)),
                       index1 = FALSE,
                       dimnames= list(row.names(Transaction),NULL))
  
  frequs <- cand %*% Transaction
  itemNum <- rowSums(cand)
  
  comp_mat <- matrix(rep(itemNum, each = ncol(Transaction)), nrow = nrow(cand), byrow = TRUE)
  support <- rowSums(frequs >= comp_mat)
  
  return(support / ncol(Transaction))
}



