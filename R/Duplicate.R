#' Find duplicate columns in a sparse matrix
#' 
#' This function is based on a c function from the arules package that finds duplicated columns
#' in a sparse matrix
#' @useDynLib ProjectApriori R_pnindex
#' @name Duplicate
#' @param mat a sparse matrix.
#' @return A boolean vector that does have one element for each column in the matrix.

Duplicate <- function(mat){
  
  # If the matrix is not a compressed aparse matrix, make it one! :)  #
  if (class(mat)[1] == "ngTMatrix"){
    mat <- sparseMatrix(i = mat@i,
                        j = mat@j,
                        giveCsparse = TRUE,
                        dim = c(nrow(mat), ncol(mat)),
                        index1 = FALSE,
                        dimnames = list(rownames(mat), NULL))
  }
  
  if (class(mat)[1] == "lgCMatrix"){
    mat <- sparseMatrix(i = mat@i,
                        p = mat@p,
                        giveCsparse = TRUE,
                        dim = c(nrow(mat), ncol(mat)),
                        index1 = FALSE,
                        dimnames = list(rownames(mat), NULL))
  }
  
  # Check whether correct class supplied since the C routine does only work with matrices of the 
  # class ngCmatrix (sparse, column oriented compressed matrices).
  if (class(mat)[1] == "ngCMatrix"){
    
    # call the c routine that give a unique ID for each unqiue column. Therefore, If 
    # a column is repeated the ID is repeated. 
    mat_col <- .Call(R_pnindex, mat, NULL, FALSE)
    
    # applying duplicated gives whether some of the IDs are repeated and therefore the
    # repeated columns. By doing this we automatically select the first columns of the 
    # repeated column (we could change this in duplicated)
    return(duplicated(mat_col))
  } else {
    
    print( "hi")
    # In this branch the supplied matrix was not of type ngTMatrix and converted 
    # or type ngCMatrix, therefore is not supported. Give Error!
    stop(paste("The class ", class(mat)[1], "is unknown to the Duplicate function"))
  }
}
