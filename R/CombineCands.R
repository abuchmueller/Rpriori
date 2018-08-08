#' Combining list of sparse incident matrix  of itemsets with different length.
#' 
#' This functions takes a list of sparce incident matrices as input. This incident may have
#'  different number or rows (different items), different number of columns 
#'  (different number of itemsets) as well es different number of items per itemset. Then these
#'   matrices are combined to a big, sparse incident matrix that does have all rows and columns
#'    of the input matrices.
#' @name CombineCands
#' @param list_input List of sparse incident matrices. These matrices may have different number 
#' of rows, columns and number of items
#' @return The result will be yet another incident matrix that will have all items
#'  rows of all input matrices and will represent all itemset in the input matrices.

CombineCands <- function(list_input){
  
  # Determine the dimensions of the result matrix
  # It should have all rows, columns from all input matrices
  
  # Collect the number of itemsets / columns in ncols.
  ncols <- 0
  
  # collect the names of the different items in items
  items <- c()
  
  # Iterate over the list and add the number of columns as well as the items.
  for (cand in list_input){
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, rownames(cand)))
  }
  
  # Create the output matrix based on the dimension calculated above.
  # The matrix is created completely empty and will be filled later.
  res_mat <- sparseMatrix(i = c(),
                          j = c(),
                          giveCsparse = FALSE,
                          dim = c(length(items), ncols),
                          dimnames = list(items, NULL))
  
  
  # Iterate again over the matrices in the input list and fill the output matrix with 
  # the correct entries from the matrices.
  
  # Initialize column counter that keeps track of which columns are currently overwritten
  # in the output matrix.
  cur_col <- 1
  
  for (cand in list_input){
    if (ncol(cand) > 0){
      
      # It is ensured that the current matrix is not empty.
      # Select only the rows from the output matrix that are also in the current matrix as well
      # as the columns in the output matrix that will represent the current matrix.
      # This selection of the output matrix can simply be overwritten by the current matrix.
      res_mat[rownames(res_mat) %in% rownames(cand), cur_col:(cur_col + ncol(cand) - 1)] <- cand
      
      # this 
      cur_col <- cur_col + ncol(cand)
    }
  }
  
  # Output the filled matrix.
  return(res_mat)
}

