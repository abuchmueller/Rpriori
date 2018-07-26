#' Make Transaction Dataset from arules to non- sparse matrix
#' 
#' Takes a object of class ... from arules and makes non-sparse matrix from it.
#' @param mat Incident matrix of the transactions
#' @return The non-sparse matrix
#' @name makeTansactionMatrix
#' 

# The following function will take a arules transaction matrix and create a normal matrix from it #
# This does not make any sense performance wise but in order to create the first version of our code it is easier to understand #
makeTansactionMatrix <- function(mat){
  
  # Get the underlying data matrix from the transaction matrix 
  mat_out <- t(as.matrix(mat@data))
  
  # Add the rownames (which do represent the different items)
  colnames(mat_out) <- mat@itemInfo$labels
  
  
  # return the  resulting matrix #
  return(mat_out)
}
