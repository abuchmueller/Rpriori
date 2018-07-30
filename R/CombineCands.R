#' Combining incidence matrices of different length
#' 
#' @name CombineCands
#' @param list_input has to be a list of matrices. These matrices should have appropriate rownames describing the different items. The items should represent 
#' the different candidates.
#' @return The result will be yet another incident matrix that will have all items rows of all input matrices and will represent all itemset in the input matrices.

CombineCands <- function(list_input){
  
  # Determine the dimensions of the result matrix #
  ncols <- 0
  items <- c()
  for (cand in list_input){
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, rownames(cand)))
  }
  
  # Build matrix #
  res_mat <- matrix(rep(FALSE, ncols * length(items)),ncol = ncols, dimnames = list(items, NULL))
  
  # Fill matrix #
  cur_col <- 1
  for (cand in list_input){
    if (ncol(cand) > 0){
    res_mat[rownames(res_mat) %in% rownames(cand), cur_col:(cur_col + ncol(cand) - 1)] <- cand
    
    cur_col <- cur_col + ncol(cand)
    }
  }

  return(res_mat)
}
