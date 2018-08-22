#' Take different input and make them a TAMatrix.
#' 
#' Takes a object of class Matrix, ngCMatrix, ngTMatrix or formel class transactions and return a
#' corresponding object of class TAMatrix. All these objects with the exception of transactions 
#' should have row names that describe the different items. The columns should represent the 
#' itemset and should be of type boolean
#' @name makeTAMatrix
#' @export
#' @param input Object of class matrix, sparse matrix or Formel class transaction (arules
#' class).
#' @return Object of class TAMatrix containing the same itemset as the input.

setGeneric('makeTAMatrix', function(input) UseMethod('makeTAMatrix'))

outputTAMatrix <- function(out_mat){
  out <- new('TAMatrix',data  = out_mat, 
             dim  = c(nrow(out_mat),ncol(out_mat)),
             items = row.names(out_mat))
  
  return(out)
}

makeTAMatrix.TAMatrix <- function(input){
  return(input)
}
  
makeTAMatrix.transactions <- function(input){
  
  # Input is of class transaction from arules
  
  # Get the underlying data matrix from the transaction matrix 
  out_mat <- as(input@data, "TsparseMatrix")
  
  # Add the rownames (which do represent the different items)
  rownames(out_mat) <- input@itemInfo$labels
  
  return(outputTAMatrix(out_mat))
}

makeTAMatrix.transactions <- function(input){
  
  # Input is of class transaction from arules
  
  # Get the underlying data matrix from the transaction matrix 
  out_mat <- as(input@data, "TsparseMatrix")
  
  # Add the rownames (which do represent the different items)
  rownames(out_mat) <- input@itemInfo$labels
  
  return(outputTAMatrix(out_mat))
}

makeTAMatrix.matrix <- function(input){
  
  # Input is a matrix 
  
  # Find positions of the true elements
  pos_true <- which(input, arr.ind = TRUE)
  
  i <- pos_true[,1]
  j <- pos_true[,2]
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  out_mat <- sparseMatrix(i = i,
                          j = j,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  return(outputTAMatrix(out_mat))
}

makeTAMatrix.data.frame <- function(input){
  
  # Input is a data.frame
  input <- as.matrix(input)
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

makeTAMatrix.ngCMatrix <- function(input){
  
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

makeTAMatrix.ngTMatrix <- function(input){
  
  # Input is already in correct sparse matrix format.
  out_mat <- input
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

makeTAMatrix.lgTMatrix <- function(input){
  
  out_mat <- sparseMatrix(i = input@i,
                          j = input@j,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

makeTAMatrix.lgCMatrix <- function(input){
  
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

makeTAMatrix.default <- function(input){
  print(paste("Object of class", class(input)[1], "cannot be coerced to TAMatrix. "))
}





#' Take different input and make them a FIMatrix
#' 
#' Takes a object of class Matrix, ngCMatrix, ngTMatrix, transactions or itemsets  and return a
#' corresponding object of class FIMatrix. The rows should represent the itemset and be named.
#' The columns should describe the itemsets. 
#' @name makeFIMatrix
#' @export
#' @param input Object of class matrix, sparse matrix,  transactions (arules
#' class) or itemsets containing the frequent itemsets.
#' @param support The support values for the provided itemsets. Should have a length corresponding 
#' to the columns of the supplied matrix. If no support is provided it has to be recalculated.
#' @param dataset Either this has to be provided or support. Please provide the initial datasets
#' based on which the frequent itemsets from input with minimal support where calculated.
#' @return 
#' 
setGeneric('makeFIMatrix', function(input, support, dataset) UseMethod('makeFIMatrix'))

outputFIMatrix <- function(mat, support){
  return(new("FIMatrix",
             data = mat,
             support = support))
}

calcsupportFIMatrix <- function(input, support, dataset){
  
  if ((missing(support) || is.null(support)) && (missing(dataset) || is.null(dataset))){
    stop("Supported has to be calculated. Please provide either the correct vector of 
         support or the dataset with a minimal support level.")
  }
  
  # Recalculate the support
  if (missing(support) || is.null(support)){
    out_support <- DetSupport(makeTAMatrix(input)@data, Transaction = makeTAMatrix(dataset),
                              same_item_num = FALSE)
  } else {
    out_support <- support
  }
  
  return(out_support)
}

makeFIMatrix.FIMatrix <- function(input, support, dataset){
  
  # input is already in the correct format
  return(input)
}

makeFIMatrix.itemsets <- function(input, support, dataset){
  
  # Extract support from itemsets object
  out_support <- input@quality[,1]
  
  # Extract and coerce sparse, compressed pattern matrix to sparse, non-compressed pattern matrix
  out_mat <- sparseMatrix(i = input@items@data@i,
                          p = input@items@data@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input@items@data), ncol(input@items@data)),
                          dimnames = list(input@items@itemInfo$labels, NULL))
  
  # Only select the items (rows) that have at least one occurence
  out_mat <- out_mat[rowSums(out_mat) > 0 ,,drop = FALSE]
  
  return(outputFIMatrix(out_mat, out_support))
}

makeFIMatrix.transactions <- function(input, support, dataset){
  
  # Input is of class transaction from arules
  
  # Get the underlying data matrix from the transaction matrix 
  out_mat <- as(input@data, "TsparseMatrix")
  
  # Add the rownames (which do represent the different items)
  rownames(out_mat) <- input@itemInfo$labels
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

makeFIMatrix.matrix <- function(input, support, dataset){

  # Input is  a matrix 
  # Find positions of the true elements
  pos_true <- which(input, arr.ind = TRUE)
  
  i <- pos_true[,1]
  j <- pos_true[,2]
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  out_mat <- sparseMatrix(i = i,
                          j = j,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}


makeFIMatrix.data.frame <- function(input, support, dataset){
  
  # Method is the same as for matrix, but coerce to matrix first.
  return(makeFIMatrix.matrix(as.matrix(input), support, dataset))
  
}

makeFIMatrix.ngCMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

makeFIMatrix.ngTMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- input
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

makeFIMatrix.lgTMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          j = input@j,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

makeFIMatrix.lgCMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dim = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}
