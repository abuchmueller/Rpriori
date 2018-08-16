#' Make Transaction Dataset from arules to sparse matrix
#' 
#' Takes a object of class Itemmatrix from arules and make it a  sparse matrix.
#' @name makeTransactionMatrix
#' @export
#' @param mat Object of class Itemmatrix form arules
#' @return The non-sparse matrix
#' 

# The following function will convert an arules transaction matrix into a normal matrix
# This does not make any sense performance wise but in order to create the first version
# of our code it is easier to understand.

makeTransactionMatrix <- function(mat){
  
  if (class(mat)[1] == "transactions"){
    # Input is a transaction from arules
    
    # Get the underlying data matrix from the transaction matrix 
    mat_out <- as(mat@data, "TsparseMatrix")
    
    # Add the rownames (which do represent the different items)
    rownames(mat_out) <- mat@itemInfo$labels
    
    # return the  resulting matrix #
    return(mat_out)
  } 
  
  if (is.matrix(mat)){
    # Input is a matrix 
    
    # Find positions of the true elements
    pos_true <- which(input_sets, arr.ind = TRUE)
    
    i <- pos_true[,1]
    j <- pos_true[,2]
    
    # Create a sparse Matrix using the sparseMatrix function from the Matrix package
    out_mat <- sparseMatrix(i = i,
                            j = j,
                            giveCsparse = FALSE,
                            dim = c(nrow(mat), ncol(mat)),
                            dimnames = list(rownames(mat), NULL))
    return(out_mat)
  }
}
