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

#' Function to create a TAMatrix from an ngTMatrix.
#' @name outputTAMatrix
#' @param out_mat Object of class ngTMatrix
#' @return Object of class TAMatrix corresponding to the ngTMatrix
#' @export
outputTAMatrix <- function(out_mat){
  out <- new('TAMatrix',data  = out_mat, 
             dim  = c(nrow(out_mat),ncol(out_mat)),
             items = row.names(out_mat))
  
  return(out)
}

#' Function to create a TAMatrix from an TAMatrix
#' @name makeTAMatrix.TAMatrix
#' @param input Object of class TAMatrix
#' @return Object of class TAMatrix containing the same itemset as the input.
#' @export
makeTAMatrix.TAMatrix <- function(input){
  return(input)
}
  
#' Function to create a TAMatrix from an object of class transaction(coming from the arules 
#' package)
#' @name makeTAMatrix.transactions
#' @param input Object of class transactions
#' @return Object of class transactions containing the same itemset as the input
#' @export
makeTAMatrix.transactions <- function(input){
  
  # Input is of class transaction from arules
  
  # Get the underlying data matrix from the transaction matrix 
  out_mat <- as(input@data, "TsparseMatrix")
  
  # Add the rownames (which do represent the different items)
  rownames(out_mat) <- input@itemInfo$labels
  
  return(outputTAMatrix(out_mat))
}


#' Function to create a TAMatrix from an object of class matrix
#' @name makeTAMatrix.matrix
#' @param input Object of class matrix
#' @return Object of class matrix containing the same itemset as the input.
#' @export
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
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  return(outputTAMatrix(out_mat))
}

#' Function to create a TAMatrix from an object of class data.frame
#' @name makeTAMatrix.data.frame
#' @param input Object of class data.frame
#' @return Object of class data.frame containing the same itemset as the input.
#' @export
makeTAMatrix.data.frame <- function(input){
  
  # Input is a data.frame
  input <- as.matrix(input)
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

#' Function to create a TAMatrix from an object of class ngCMatrix
#' @name makeTAMatrix.ngCMatrix
#' @param input Object of class ngCMatrix
#' @return Object of class data.frame containing the same itemset as the input.
#' @export
makeTAMatrix.ngCMatrix <- function(input){
  
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

#' Function to create a TAMatrix from an object of class ngTMatrix
#' @name makeTAMatrix.ngTMatrix
#' @param input Object of class ngTMatrix
#' @return Object of class data.frame containing the same itemset as the input.
#' @export
makeTAMatrix.ngTMatrix <- function(input){
  
  # Input is already in correct sparse matrix format.
  out_mat <- input
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

#' Function to create a TAMatrix from an object of class lgTMatrix
#' @name makeTAMatrix.lgTMatrix
#' @param input Object of class lgTMatrix
#' @return Object of class data.frame containing the same itemset as the input.
#' @export
makeTAMatrix.lgTMatrix <- function(input){
  
  out_mat <- sparseMatrix(i = input@i,
                          j = input@j,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

#' Function to create a TAMatrix from an object of class lgCMatrix
#' @name makeTAMatrix.lgCMatrix
#' @param input Object of class lgCMatrix
#' @return Object of class data.frame containing the same itemset as the input.
#' @export
makeTAMatrix.lgCMatrix <- function(input){
  
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # From here on same procedure as input would be matrix.
  return(makeTAMatrix.matrix(input))
}

#' Default method for not implemented classes
#' @name makeTAMatrix.default
#' @param input Object of uknown class
#' @return Error message
#' @export
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
#' @return Object of class FIMatrix representing the input object.
#' 
setGeneric('makeFIMatrix', function(input, support, dataset) UseMethod('makeFIMatrix'))


#' Function to create a FIMatrix from an ngTMatrix as well as the corresponding vector of support.
#' @name outputFIMatrix
#' @param mat Object of class ngTMatrix
#' @param support Vector containing the coresponding support values for the frequent itemsets in
#' mat
#' @return Object of class FIMatrix corresponding to the ngTMatrix and the support vector
#@export
outputFIMatrix <- function(mat, support){
  return(new("FIMatrix",
             data = mat,
             support = support))
}

#' Calculate support if missing.
#' 
#' This function calculate the support for a frequent itemset with a transactions set if the 
#' support vector is missing and otherwise otherwise just return the support vector
#' @name calcsupportFIMatrix
#' @param input The frequent Itemsets as a ngTMatrix
#' @param support The support vector, if missing the support is calculated for the frequent itemsets
#' based on the transaction set in dataset.
#' @param dataset dataset containing the transaction data. May be of all classes that can be coerced
#' to TAMatrix by the function MakeTAMatrix.
#' @return Vector of support values.
#' @export
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

#' Function to create a FIMatrix from an FIMatrix
#' @name makeFIMatrix.FIMatrix
#' @param input Object of class FIMatrix
#' @param support Support values for the FIMatrix
#' @param dataset Underlying transactions. Has to be supplied if the support vector is missing.
#' @return Object of class FIMatrix containing the same itemset as the input.
#' @export
makeFIMatrix.FIMatrix <- function(input, support, dataset){
  
  # input is already in the correct format
  return(input)
}


#' Function to create a FIMatrix from an itemsets
#' @name makeFIMatrix.itemsets
#' @param input Object of class itemsets
#' @param support Support values for the itemsets
#' @param dataset Underlying transactions. Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.itemsets <- function(input, support, dataset){
  
  # Extract support from itemsets object
  out_support <- input@quality[,1]
  
  # Extract and coerce sparse, compressed pattern matrix to sparse, non-compressed pattern matrix
  out_mat <- sparseMatrix(i = input@items@data@i,
                          p = input@items@data@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input@items@data), ncol(input@items@data)),
                          dimnames = list(input@items@itemInfo$labels, NULL))
  
  # Only select the items (rows) that have at least one occurence
  out_mat <- out_mat[rowSums(out_mat) > 0 ,,drop = FALSE]
  
  return(outputFIMatrix(out_mat, out_support))
}


#' Function to create a FIMatrix from an transactions
#' @name makeFIMatrix-transactions
#' @param input Object of class transactions
#' @param support Support values for the transactions
#' @param dataset Underlying transactions. Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
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

#' Function to create a FIMatrix from an matrix
#' @name makeFIMatrix.matrix
#' @param input Object of class matrix
#' @param support Support values for the matrix
#' @param dataset Underlying matrix Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
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
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

#' Function to create a FIMatrix from an data.frame
#' @name makeFIMatrix.data.frame
#' @param input Object of class data.frame
#' @param support Support values for the data.frame
#' @param dataset Underlying data.frame Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.data.frame <- function(input, support, dataset){
  
  # Method is the same as for matrix, but coerce to matrix first.
  return(makeFIMatrix.matrix(as.matrix(input), support, dataset))
  
}

#' Function to create a FIMatrix from an ngCMatrix
#' @name makeFIMatrix.ngCMatrix
#' @param input Object of class ngCMatrix
#' @param support Support values for the ngCMatrix
#' @param dataset Underlying data.frame Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.ngCMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

#' Function to create a FIMatrix from an ngTMatrix
#' @name makeFIMatrix.ngTMatrix
#' @param input Object of class ngTMatrix
#' @param support Support values for the ngTMatrix
#' @param dataset Underlying data.frame Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.ngTMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- input
  
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

#' Function to create a FIMatrix from an lgTMatrix
#' @name makeFIMatrix.lgTMatrix
#' @param input Object of class lgTMatrix
#' @param support Support values for the lgTMatrix
#' @param dataset Underlying data.frame Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.lgTMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from the Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          j = input@j,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}

#' Function to create a FIMatrix from an lgCMatrix
#' @name makeFIMatrix.lgCMatrix
#' @param input Object of class lgCMatrix
#' @param support Support values for the lgCMatrix
#' @param dataset Underlying data.frame Has to be supplied if the support vector is missing and
#' has to be calculated.
#' @return Object of class FIMatirx containing correct support values
#' @export
makeFIMatrix.lgCMatrix <- function(input, support, dataset){
  
  # Input is  a sparse, compressed pattern matrix
  
  # Create a sparse Matrix using the sparseMatrix function from thve Matrix package
  # Input is a compressed, sparse matrix
  out_mat <- sparseMatrix(i = input@i,
                          p = input@p,
                          index1 = FALSE,
                          giveCsparse = FALSE,
                          dims = c(nrow(input), ncol(input)),
                          dimnames = list(rownames(input), NULL))
  # Determine support 
  out_support <- calcsupportFIMatrix(input, support, dataset)
  
  return(outputFIMatrix(out_mat, out_support))
}
