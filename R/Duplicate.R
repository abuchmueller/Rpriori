#' @useDynLib ProjectApriori R_pnindex

Duplicate <- function(mat){
  
  # If the matrix is not a compressed matrix, create a compressed matrix #
  if (class(mat)[1] == "ngTMatrix"){
    mat <-   cand <- sparseMatrix(i = mat@i,
                                  j = mat@j,
                                  giveCsparse = TRUE,
                                  dim = c(nrow(mat), ncol(mat)),
                                  index1 = FALSE,
                                  dimnames = list(rownames(mat), NULL))
  }
  
  # Check whether correct class supplied
  if (class(mat)[1] == "ngCMatrix"){
    mat_col <- .Call(R_pnindex, mat, NULL, FALSE)
    return(duplicated(mat_col))
  } else {
    stop(paste("The class ", class(mat)[1], "is unknown to the Duplicate function"))
  }
}
