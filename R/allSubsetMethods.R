
########################################################
# This file stores Subsetting and other  matrix methods 
# for all classes #
########################################################
#' @include allClasses.R



######################
# Itemsets: TAMatrix #
######################

# For itemsets the logic for subsetting is the following:
# for the items only rowise selection (via i) is relevant, for dim and the matrix row and 
# columnwise selection is relevant.
setGeneric("items", function(x) 
  standardGeneric("items") )

setMethod("items",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(rownames(x@data))
          })


setMethod("colSums",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(colSums(x@data))
          })

setMethod("rowSums",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(rowSums(x@data))
          })

setMethod("ncol",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(ncol(x@data))
          })

setMethod("nrow",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(nrow(x@data))
          })


setMethod("[",  signature = signature(x = "TAMatrix"), 
          function(x, i = NULL, j = NULL) {
            
  # If i or j are missing, all rows / columns should be selected.
  if (missing(i)){
    
    if (is.logical(j)){
      j <- which(j)
    }
    
    return(new('TAMatrix',
               data = x@data[, j, drop = FALSE],
               dim = c(nrow(x@data), length(j)),
               items = x@items))
  }
            
  if (missing(j)){
    
    if (is.logical(i)){
      i <- which(i)
    }

    return(new('TAMatrix',
               data = x@data[i, , drop = FALSE],
               dim = c(length(i), ncol(x@data)),
               items = x@items[i, drop = FALSE]))
  }

  if (missing(i) && missing(j)){
    return(x)
  }
            
  # If i or j is logical than make it to an integer of the true positions
  if (is.logical(i)){
    i <- which(i)
  }
            
  if (is.logical(j)){
    j <- which(j)
  }
  
  return(new('TAMatrix',
         data = x@data[i, j, drop = FALSE],
         dim = c(length(i), length(j)),
         items = x@items[i, drop = FALSE]))
})



###############################
# Frequent Itemsets: FIMatrix #
###############################

setMethod("items",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(rownames(x@data))
          })

setMethod("colSums",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(colSums(x@data))
          })

setMethod("rowSums",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(rowSums(x@data))
          })

setMethod("colSums",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(colSums(x@data))
          })

setMethod("ncol",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(ncol(x@data))
          })

setMethod("nrow",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(nrow(x@data))
          })

setMethod("[",  signature = signature(x = "FIMatrix"), 
          function(x, i, j) {
            
            # If the matrix does not have row or columns return an empty matrix
            if (nrow(x@data) == 0 || ncol(x@data) == 0){
              return(new('FIMatrix',
                         data = x@data[0, 0, drop = FALSE],
                         support = x@support[0, drop = FALSE]))
            }
            
            # If i is missing use all rows of the input 
            if (missing(i)){
              i <- 1:nrow(x@data)
            }
            
            # If j is missing use all columns of the input 
            if (missing(j)){
              j <- 1:ncol(x@data)
            }
            
            # if i, j is logical replace it by the positions of the true values
            if (is.logical(i)){
              i <- which(i)
              i <- as.numeric(i)
            }
            
            if (is.logical(j)){
              j <- which(j)
              j <- as.numeric(j)
            }
            
            if (length(i) == 0 || length(j) == 0){
              return(new('FIMatrix',
                         data = x@data[0, 0, drop = FALSE],
                         support = x@support[0, drop = FALSE]))
            }
            

            return(new('FIMatrix',
                       data = x@data[i, j, drop = FALSE],
                       support = x@support[j, drop = FALSE]))
            
          })



################
# Rules: Rules #
################
setMethod("rowSums",  signature = signature(x = "Rules"), 
          function(x, lhs = TRUE){
            if (lhs){
              return(rowSums(x@lhs))
            } else {
              return(rowSums(x@rhs))
            }
          })

setMethod("colSums",  signature = signature(x = "Rules"), 
          function(x, lhs = TRUE){
            if (lhs){
              return(colSums(x@lhs))
            } else {
              return(colSums(x@rhs))
            }
          })

setMethod("items",  signature = signature(x = "Rules"), 
          function(x){
            return(rownames(x@lhs))
          })

setMethod("ncol",  signature = signature(x = "Rules"), 
          function(x){
            return(ncol(x@lhs))
          })

setMethod("nrow",  signature = signature(x = "Rules"), 
          function(x){
            return(nrow(x@lhs))
          })

setMethod("[",  signature = signature(x = "Rules"), 
          function(x, i, j) {
            
            # If the matrix does not have row or columns return an empty matrix
            if (nrow(x@lhs) == 0 || ncol(x@lhs) == 0 || nrow(x@rhs) == 0 || ncol(x@rhs) == 0){
              return(new('Rules',
                         lhs = x@lhs[0,0,drop = FALSE],
                         rhs = x@rhs[0,0,drop = FALSE],
                         support = x@support[0, drop = FALSE],
                         confidence = x@confidence[0, drop = FALSE],
                         lift = x@lift[0, drop = FALSE],
                         leverage = x@leverage[0, drop = FALSE],
                         itemsetID = x@itemsetID[0, drop = FALSE],
                         FrequentItemsets = x@FrequentItemsets))
            }
            
            # If i is missing use all rows of the input 
            if (missing(i)){
              i <- 1:nrow(x@lhs)
            }
            
            # If j is missing use all columns of the input 
            if (missing(j)){
              j <- 1:ncol(x@lhs)
            }
            
            # if i, j is logical replace it by the positions of the true values
            if (is.logical(i)){
              i <- which(i)
              i <- as.numeric(i)
            }
            
            if (is.logical(j)){
              j <- which(j)
              j <- as.numeric(j)
            }
            
            if (length(i) == 0 || length(j) == 0){
              return(new('Rules',
                         lhs = x@lhs[0,0,drop = FALSE],
                         rhs = x@rhs[0,0,drop = FALSE],
                         support = x@support[0, drop = FALSE],
                         confidence = x@confidence[0, drop = FALSE],
                         lift = x@lift[0, drop = FALSE],
                         leverage = x@leverage[0, drop = FALSE],
                         itemsetID = x@itemsetID[0, drop = FALSE],
                         FrequentItemsets = x@FrequentItemsets))
            }
            return(new('Rules',
                       lhs = x@lhs[i,j,drop = FALSE],
                       rhs = x@rhs[i,j,drop = FALSE],
                       support = x@support[j, drop = FALSE],
                       confidence = x@confidence[j, drop = FALSE],
                       lift = x@lift[j, drop = FALSE],
                       leverage = x@leverage[j, drop = FALSE],
                       itemsetID = x@itemsetID[j, drop = FALSE],
                       FrequentItemsets = x@FrequentItemsets)) 
            
          })

