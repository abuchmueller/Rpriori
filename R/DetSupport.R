#' Determine the support of a single candidate set within the transactions
#' 
#' @param cand This should represent the candidates the support has to be checked. It has to be an Incidence matrix where the rows do describe the different items.
#' Also it has to have rownames that are a subset of the rownames of Transaction.
#' @param Transaction This should be all the transaction from which the occurence (support) of the candidate should be checked. It has to be a incident matrix 
#' with named rows that do represent the different items.
#' @name DetSupport_row
#' @return The support of the candidate wihtin the transactions as floating-point value.

DetSupport_row <- function(cand, Transaction){
  
  # If the input is a vector, make it a matrix instead #
  if (is.vector(mat_rel[,1])){
    cand <- as.matrix(cand)
  }
  
  # if Transaction and cand do not have the same rows, this vector shows which rows from Transaction are needed to compare to cand.
  rel_rows <- rownames(Transaction) %in% rownames(cand)
  
  
  if (all(rel_rows)){
    
    # Here all rows in Transaction are in cand and we can compare them directly.
    # The logit is that I apply the specified funcion to each col of Transaction.
    # For each row I check whether the and operator if the fields are the same and return whether the number
    # of items in cand does represent the number of matches from Transaction. If this is true, then the col of Transaction is
    # or does at least contain the itemset from cand.
    matches <- apply(Transaction,2, function(cur_col){
      return(sum(cand & cur_col) == sum(cand))
    })
    sum(matches) / ncol(Transaction)
  } else {
    
    # Here the logit is the as in the first branch of the if-statement but I select only the rows from Transaction that are in cand.
    matches <- apply(Transaction[rel_rows,],2, function(cur_col){
      return(sum(cand & cur_col) == sum(cand))
    })
    sum(matches) / ncol(Transaction)
  }
}


#' Determine the support of a matrix of candidate set within the transactions
#' 
#' @param cand This should represent the matrix of  candidates for which the support has to be checked.
#' It has to be an Incidence matrix where the rows do describe the different items.
#' Each columns should represent one candidate.
#' Also it has to have rownames that are a subset of the rownames of Transaction.
#' @param Transaction This should be all the transaction from which the occurence (support) of the candidate should be checked. It has to be a incident matrix 
#' with named rows that do represent the different items.
#' @name DetSupport_row
#' @return The support of the candidates wihtin the transactions as a vector floating-point value.

DetSupport <- function(candmat, Transaction){
  return(apply(candmat, 2 ,DetSupport_row, Transaction = Transaction))
}




