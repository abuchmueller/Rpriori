#' Give only the unique columns of a sparse matrix.
#' 
#' @name GiveUniqueCol
#' @param mat Sparse matrix, where the rows are the items and the columns are the itemsets of type ngCmatrix
#' @export
#' @return Returns only non-duplicate columns of matrix. Ordering might be changed.

GiveUniqueCol <- function(mat){
  return(mat[,!Duplicate(mat), drop = FALSE])
}


