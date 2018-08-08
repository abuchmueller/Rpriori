#' Determine the support of a matrix of candidate set within the transactions.
#' 
#' @name DetSupport
#' @param cand This should represent the matrix of  candidates for which the support has to be
#'  checked.
#' It has to be an sparse incidence matrix where the rows do describe the different items.
#' Each column should represent one candidate.
#' Also it has to have rownames that are a subset of the rownames of Transaction.
#' @param Transaction This should be all the transaction from which the occurence (support)
#'  of the candidate should be checked. It has to be a incident matrix 
#' with named rows that do represent the different items.
#' @return The support of the candidates wihtin the transactions
#'  as a vector of floating-point value.
#' @import Matrix
DetSupport <- function(cand, Transaction){
  
  # if empty candidates are supplied than return an empty vector as support.
  if (ncol(cand) == 0 ){
    return(c(1)[0])
  }  
  
  # We need to multiply cand later on with the Transaction matrix. But it can be the case
  # that the cand matrix does only have subsets of the columns of the Transaction matrix.
  # Therefore cand_filled is created that does have all rows of Transaction but the same columns
  # and itemsets as cand. Obviously many rows are zero.
  
  cand_filled <- sparseMatrix(i = c() , j = c(),
                              dims = c(nrow(Transaction), ncol(cand)),
                              index1 = FALSE,
                              giveCsparse = TRUE,
                              dimnames= list(row.names(Transaction),NULL))
  
  
  # Overwrite the correct values in cand_filled so that it does contain the itemset from cand.
  cand_filled[rownames(Transaction) %in% rownames(cand),] <- cand
  
  # make cand from logical to numeric and also transpose is since we do need to 
  # multiply the transposed later on.
  cand <- t(cand_filled * 1)
  
  # Make Transactions matrix to integer sparse matrix #
  Transaction <- sparseMatrix(i = Transaction@i , j = Transaction@j,
                              x = rep(1, length(Transaction@i)),
                              dims = c(nrow(Transaction), ncol(Transaction)),
                              index1 = FALSE,
                              giveCsparse = TRUE,
                              dimnames= list(row.names(Transaction),NULL))
  
  
  # The matrix product of cand with Transaction reveals how many items a certain
  # itemset had that matched with the candidate. If this are as many as the itemset hat obviously
  # that itemset did contain the candidate.
  frequs <- cand %*% Transaction
  
  # Here I count how man items each candidate had.
  itemNum <- rowSums(cand)
  
  # When there are as many machtes in a column of frequs as items in the respective candidates than
  # this candidate was part of that set.
  comp <- frequs >= itemNum
  
  # the support (absolut count here) is represented by how often we had a match with the candates
  # in all itemsets of Transaction
  
  support <- rowSums(comp)
  
  # return the relative support.
  return(support / ncol(Transaction))
}
