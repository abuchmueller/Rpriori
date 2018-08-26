#' Combining list of FIMatrix containing frequent itemsets with different length.
#' 
#' This functions takes a list of FIMatrices matrices as input. The FIMatrices may have
#'  different number or rows (different items), different number of columns 
#'  (different number of itemsets) as well es different number of items per itemset. Then these
#'   matrices are combined to a big, sparse incident matrix that does have all rows and columns
#'    of the input matrices.
#' @name CombineFIMatrix
#' @param list_input List of sparse incident matrices. These matrices may have different number 
#' of rows, columns and number of items
#' @return Another object of class FIMatrix that contains all the input Itemsets.

CombineFIMatrix <- function(list_input){
  
  # Determine the dimensions of the result matrix
  # It should have all rows, columns from all input matrices
  
  # Collect the number of itemsets / columns in ncols.
  ncols <- 0
  
  # collect the names of the different items in items
  items <- c()
  
  # Iterate over the list and add the number of columns as well as the items.
  for (cand in list_input){
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, items(cand)))
  }
  
  # Create the output matrix based on the dimension calculated above.
  # The matrix is created completely empty and will be filled later.
  res_mat <- new('FIMatrix',
                 data = sparseMatrix(i = c(),
                          j = c(),
                          giveCsparse = FALSE,
                          dims = c(length(items), ncols),
                          dimnames = list(items, NULL)),
                 support = rep(0, ncols))
  
  
  # Iterate again over the matrices in the input list and fill the output matrix with 
  # the correct entries from the matrices.
  
  # Initialize column counter that keeps track of which columns are currently overwritten
  # in the output matrix.
  cur_col <- 1
  
  for (cand in list_input){
    if (ncol(cand@data) > 0){
      
      # It is ensured that the current matrix is not empty.
      # Select only the rows from the output matrix that are also in the current matrix as well
      # as the columns in the output matrix that will represent the current matrix.
      # This selection of the output matrix can simply be overwritten by the current matrix.
      res_mat@data[rownames(res_mat@data) %in% rownames(cand@data),
              cur_col:(cur_col + ncol(cand@data) - 1)] <- cand@data
      
      # Here we combine the support of the of the input frequent itemsets
      res_mat@support[cur_col:(cur_col + ncol(cand@data) - 1)] = cand@support
      
      # this 
      cur_col <- cur_col + ncol(cand@data)
    }
  }
  
  # Output the filled matrix.
  return(res_mat)
}


#' Combining list of Rules containing Rules of different length.
#' 
#' @name CombineRules
#' @param list_input List of Rules. These Rules may have different number 
#' of rows, columns and number of items
#' @return The result will be yet another Rules Object that will contain all the rules from the 
#' input rules but in one object.

CombineRules <- function(list_input){
  
  # Determine the dimensions of the result matrix
  # It should have all rows, columns from all input matrices
  
  # Collect the number of itemsets / columns in ncols.
  ncols <- 0
  
  # collect the names of the different items in items
  items <- c()
  
  # Iterate over the list and add the number of columns as well as the items.
  for (cand in list_input){
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, items(cand)))
  }
  
  # Create the output matrix based on the dimension calculated above.
  # The matrix is created completely empty and will be filled later.
  res_mat <- new('Rules',
                 lhs = sparseMatrix(i = c(),
                                    j = c(),
                                    giveCsparse = FALSE,
                                    dims = c(length(items), ncols),
                                    dimnames = list(items, NULL)),
                 rhs = sparseMatrix(i = c(),
                                    j = c(),
                                    giveCsparse = FALSE,
                                    dims = c(length(items), ncols),
                                    dimnames = list(items, NULL)),
                 support = rep(0, ncols),
                 confidence = rep(0, ncols),
                 lift = rep(0, ncols),
                 leverage = rep(0, ncols),
                 itemsetID = rep(0, ncols),
                 FrequentItemsets = list_input[[1]]@FrequentItemsets)
                
  
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
      res_mat@lhs[items(res_mat) %in% rownames(cand@lhs),
                   cur_col:(cur_col + ncol(cand) - 1)] <- cand@lhs
      
      res_mat@rhs[items(res_mat) %in% rownames(cand@lhs),
                  cur_col:(cur_col + ncol(cand) - 1)] <- cand@rhs
      
      # Here we combine the support of the of the input frequent itemsets
      res_mat@support[cur_col:(cur_col + ncol(cand) - 1)] = cand@support
      
      # Here we combine the confidence of the of the input frequent itemsets
      res_mat@confidence[cur_col:(cur_col + ncol(cand) - 1)] = cand@confidence
      
      # Here we combine the lift of the of the input frequent itemsets
      res_mat@lift[cur_col:(cur_col + ncol(cand) - 1)] = cand@lift
      
      # Here we combine the leverage of the of the input frequent itemsets
      res_mat@leverage[cur_col:(cur_col + ncol(cand) - 1)] = cand@leverage
      
      # Here we combine the itemsetID of the of the input frequent itemsets
      res_mat@itemsetID[cur_col:(cur_col + ncol(cand) - 1)] = cand@itemsetID
      
      # this 
      cur_col <- cur_col + ncol(cand)
    }
  }
  
  # Output the filled matrix.
  return(res_mat)
}








