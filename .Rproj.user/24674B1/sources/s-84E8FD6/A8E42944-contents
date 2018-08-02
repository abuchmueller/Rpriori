#' Give only the unique columns of a sparse matrix.
#' 
#' @name GiveUniqueCol
#' @param mat Sparse matrix, where the rows are the items and the columns are the itemsets of type ngCmatrix
#' @return Returns only non-duplicate columns of matrix. Ordering might be changed.

GiveUniqueCol <- function(mat){

  rel_col <- rep(TRUE, ncol(mat))
  cols_lost <- 0
  for (i in 1:ncol(mat)){
    rel <- TRUE
    curr_col <- i + 1
    while (rel & curr_col < (ncol(mat) + 1)){
      if (all(mat[,i] == mat[,rel_col][,curr_col - cols_lost])){
        # We found a duplicate #
        rel <- FALSE
        rel_col[i] <- FALSE
        cols_lost <- cols_lost + 1
        
      }
      curr_col <- curr_col + 1
    }
  }
  return(mat[,rel_col, drop = FALSE])
}
