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
#' @param Itemsetsize If all candidate
#' @return The support of the candidates wihtin the transactions
#'  as a vector of floating-point value.
#' @import Matrix
DetSupport <- function(cand, Transaction, same_item_num = FALSE){
  
  # if empty candidates are supplied than return an empty vector as support.
  if (ncol(cand) == 0 ){
    return(c(1)[0])
  }  
  
  # We need to multiply Transaction with cand later on. Therefore they have to have the same
  # rows. Only the rows are relevant that are also in Transaction.
  Transaction <- Transaction[row.names(Transaction) %in% row.names(cand),,drop = FALSE]
  
  
  # make cand from logical to numeric and also transpose is since we do need to 
  # multiply the transposed later on.
  cand <- t(cand * 1)
  
  # Make Transactions matrix to integer sparse matrix #
  Transaction <- Transaction * 1
  
  # The matrix product of cand with Transaction reveals how many items a certain
  # itemset had that matched with the candidate. If this are as many as the itemset hat obviously
  # that itemset did contain the candidate.
  frequs <- cand %*% Transaction
  
  if (! same_item_num){
    # If the items could have different number of items we have to determine the number of items 
    # and compare the matrix later to a vector.
    
    # Here I count how man items each candidate had.
    itemNum <- rowSums(cand)
    
    # When there are as many machtes in a column of frequs as items in the respective candidates than
    # this candidate was part of that set.
    comp <- frequs == itemNum
  } else {
    # When all items have the same size we can just use the size of the first one as a dummy and 
    # then can compare the matrix to a single value.
    
    # Here I count how man items each candidate had as 
    itemNum <- sum(cand[1,])
    
    # When there are as many machtes in a column of frequs as items in the respective candidates than
    # this candidate was part of that set.
    comp <- frequs == itemNum
  }
  
  
  # the support (absolut count here) is represented by how often we had a match with the candates
  # in all itemsets of Transaction
  
  support <- rowSums(comp)
  
  # return the relative support.
  return(support / ncol(Transaction))
}




