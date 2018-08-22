#' Take different input and make them a TAMatrix.
#' 
#' Takes a object of class Matrix, ngCMatrix, ngTMatrix or formel class transactions and return a
#' corresponding object of class TAMatrix. All these objects with the exception of transactions 
#' should have row names that describe the different items. The columns should represent the 
#' itemset and should be of type boolean
#' @name makeTAMatrix
#' @export
#' @param mat Object of class matrix, sparse matrix or Formel class transaction (arules
#' class).
#' @return Object of class TAMatrix containing the same itemset as the input.
#' 

# The following function will convert an arules transaction matrix into a normal matrix
# This does not make any sense performance wise but in order to create the first version
# of our code it is easier to understand.

makeTAMatrix <- function(input){
  
  # If already a TAMatrix was provided just return it.
  if(class(input)[1] == 'TAMatrix'){
    return(input)
  }
  
  if (class(input)[1] == "transactions"){
    
    # Input is of class transaction from arules
    
    # Get the underlying data matrix from the transaction matrix 
    out_mat <- as(input@data, "TsparseMatrix")
    
    # Add the rownames (which do represent the different items)
    rownames(out_mat) <- input@itemInfo$labels
    
  } else {
    if (is.matrix(input) || is.data.frame(input)){
    
    if(is.data.frame(input)){
      input <- as.matrix(input)
    }  
      
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
    } else {
      if (class(input)[1] == "ngCMatrix"){
        
        # Input is a compressed, sparse matrix
        out_mat <- sparseMatrix(i = input@i,
                                p = input@p,
                                index1 = FALSE,
                                giveCsparse = FALSE,
                                dim = c(nrow(input), ncol(input)),
                                dimnames = list(rownames(input), NULL))
        
      } else {
        if ((class(input)[1] == "ngTMatrix")){
          
          # Input is a sparse matrix
          out_mat <- input
          
        } else { 
          
          # Input is a logical, sparse, non-compressed matrix
          
          if ((class(input)[1] == "lgTMatrix")){
            out_mat <- sparseMatrix(i = input@i,
                                    j = input@j,
                                    index1 = FALSE,
                                    giveCsparse = FALSE,
                                    dim = c(nrow(input), ncol(input)),
                                    dimnames = list(rownames(input), NULL))
            
            
          } else {
            if ((class(input)[1] == "lgCMatrix")){
              
              out_mat <- sparseMatrix(i = input@i,
                                      p = input@p,
                                      index1 = FALSE,
                                      giveCsparse = FALSE,
                                      dim = c(nrow(input), ncol(input)),
                                      dimnames = list(rownames(input), NULL))
            } else {
              
              # input is non of the types above, abort execution and give error.
              stop(paste("Class", class(input)[1], "currently not supported by Project_Apriori"))
            }
          }
        }
      }
    } 
  }
  
  
 # Create Object of class TAMatrix #
  out <- new('TAMatrix',data  = out_mat, 
             dim  = c(nrow(out_mat),ncol(out_mat)),
             items = row.names(out_mat))
  
  return(out)
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

makeFIMatrix <- function(input, support, dataset){
  
  # In the case a FIMatrix was provided just return it #
  if (class(input)[1] == "FIMatrix"){
    return(input)
  }
  
  
  if (class(input)[1] != "itemsets"){
    
    # In every other input class except itemsets the support has either to be provided via
    # paramter support or to be recalculated manually based on the provided dataset and the minimal
    # support value.
    
    # Support has either to be provided in support or to be recalculated.
    if (missing(support) && missing(dataset)){
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
  }
  
  # Now coerce the input type to a appropriate sparse matrix
  if (class(input)[1] == 'itemsets'){
    
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
    
  } else {
    if (class(input)[1] == "transactions"){
      
      # Input is of class transaction from arules
      
      # Get the underlying data matrix from the transaction matrix 
      out_mat <- as(input@data, "TsparseMatrix")
      
      # Add the rownames (which do represent the different items)
      rownames(out_mat) <- input@itemInfo$labels
      
    } else {
      if (is.matrix(input) || is.data.frame(input)){
        
        if(is.data.frame(input)){
          input <- as.matrix(input)
        }  
        
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
      } else {
        if (class(input)[1] == "ngCMatrix"){
          
          # Input is a compressed, sparse matrix
          out_mat <- sparseMatrix(i = input@i,
                                  p = input@p,
                                  index1 = FALSE,
                                  giveCsparse = FALSE,
                                  dim = c(nrow(input), ncol(input)),
                                  dimnames = list(rownames(input), NULL))
          
        } else {
          if ((class(input)[1] == "ngTMatrix")){
            
            # Input is a sparse matrix
            out_mat <- input
            
          } else { 
            
            # Input is a logical, sparse, non-compressed matrix
            
            if ((class(input)[1] == "lgTMatrix")){
              out_mat <- sparseMatrix(i = input@i,
                                      j = input@j,
                                      index1 = FALSE,
                                      giveCsparse = FALSE,
                                      dim = c(nrow(input), ncol(input)),
                                      dimnames = list(rownames(input), NULL))
              
              
            } else {
              if ((class(input)[1] == "lgCMatrix")){
                
                out_mat <- sparseMatrix(i = input@i,
                                        p = input@p,
                                        index1 = FALSE,
                                        giveCsparse = FALSE,
                                        dim = c(nrow(input), ncol(input)),
                                        dimnames = list(rownames(input), NULL))
              } else {
                
                # input is non of the types above, abort execution and give error.
                stop(paste("Class", class(input)[1], "currently not supported by Project_Apriori"))
              }
            }
          }
        }
      } 
    }
  }
    
  
  return(new("FIMatrix",
      data = out_mat,
      support = out_support))
}
