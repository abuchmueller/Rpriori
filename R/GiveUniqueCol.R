#' Give only the unique columns of a sparse matrix.
#' 
#' @name GiveUniqueCol
#' @param mat Sparse incident matrix, where the rows are the items and the columns
#'  are the itemsets of type ngCmatrix
#' @export
#' @return Returns only non-duplicate columns of matrix. Ordering might be changed.

GiveUniqueCol <- function(mat){
  
  # This fuction just relies on the Duplicate function that returns a boolean vector indicating
  # whether a certain column in the matrix is a duplicate. We only select non-duplicate columns
  return(mat[,!Duplicate(mat), drop = FALSE])
}


