# ----------------------------------------------------------------------------- #
# ------------------- All methods for the classes. ---------------------------- #
# ----------------------------------------------------------------------------- #

#' @import methods
#' @include allClasses.R

# ----------------------------------------------------------------------------- #
# ------------------ All methods for class TAMATRIX --------------------------- #
# ----------------------------------------------------------------------------- #

#' Determine the number of items in a TAMatrix
#' 
#' The length function for the TAMatrix returns the number of items in that TAMatrix.
#' @name length-TAMatrix
#' @rdname length-TAMatrix
#' @param x Object of class TAMatrix
#' @aliases length-TAMatrix length,TAMatrix-method
#' @return Number of  items within the TAMatrix
#' @export 

setMethod("length", "TAMatrix", function(x) {
  x@dim[1]
})

#' Definition of print method for TAMatrix
#' 
#' The print function prints out all items of the TAMatrix with their respective counts. There are
#' sorted descending by these counts
#' @name print-TAMatrix
#' @rdname print-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @param descending Starting with the highest or lowest count?
#' @aliases print-TAMatrix print,TAMatrix-method
#' @return The frequent itemsets ordererd by their occurence
#' 
setMethod("print", "TAMatrix", function(x, descending = TRUE) {
  
  #collect all itemnames, print them by their frequency in a descending order
  print(data.frame(frequency = sort(rowSums(x@data), decreasing = descending)))
  
})


#' Definition of show method for TAMatrix
#' 
#' The show function prints out the number of items in the TAMatrix
#' @name show-TAMatrix
#' @rdname show-TAMatrix
#' @export  
#' @param object Object of class TAMatrix
#' @aliases show-TAMatrix show,TAMatrix-method
#' @return Short message stating the number of items.
setMethod("show", "TAMatrix", function(object) {
  
  n <- length(object)
  cat("Found", n, "items. Use the print() to display\n")
  
})

#' Summary for FI-matrices.
#' 
#' The sumamary function gives general information about the TAMatrix such as the density or the 
#' distribution of the length of the itemsets
#' @name summary-TAMatrix
#' @rdname summary-TAMatrix
#' @export  
#' @param object Object of class TAMatrix
#' @aliases summary-TAMatrix summary,TAMatrix-method
#' @return Summary information about the TAMatrix
#' 

setMethod("summary", signature(object = "TAMatrix"), function(object) {
  
  #matrix density
  TAM.density <- sum(colSums(object@data))  / (nrow(object@data) * ncol(object@data))
  TAM.density <- round(TAM.density, 4)
  
  #Overview over transactions matrix
  cat("\n")
  cat("Transaction database in binary sparse matrix representation \n with", 
      nrow(object@data), "rows (items) and \n", 
      ncol(object@data), "columns (itemsets/transactions) and \n a density of",
      TAM.density, "(sparsity:", paste0(1 - TAM.density, ")"))
  cat("\n")
  
  #top 8 most frequent items
  cat("\n")
  cat("Most frequent items: \n" )
  print(sort(rowSums(object@data), decreasing = T)[1:8])
  cat("\n")
  
  #distribution of itemset lengths
  cat("Distribution of itemset length:\n")
  print(table(colSums(object@data)))
  cat("\n")
  
  #summary statistics on itemset lengths
  print(summary(colSums(object@data)))
  cat("\n")
  
})

#distribution of itemset lengths as a histogram
setMethod("plot", signature(x = "TAMatrix"), function(x) {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one item")
  } else {

  #determine maximum length of itemsets to obtain number breakpoints
  max.itemlength <- max(colSums(x@data))
  
  hist(colSums(x@data), 
       breaks = max.itemlength + 1, 
       main = "Distribution of itemset lengths", xlab = "Itemset length", 
       col = "lightblue")
  }
  
})




# #ggplot2 implementation
# setMethod("qplot", signature(x = "TAMatrix"), function(x) {
#   
#   #dataframe needed for ggplot
#   df <- data.frame(itemset_length = colSums(x@data))
#   
#   ggplot(df, aes(df$itemset_length)) + 
#     geom_bar() +
#     labs(title = "Histogram of itemset lengths", x = "Itemset length") +
#     theme(plot.title = element_text(hjust = 0.5))
#   
# })


#' Extracing the names of the items. 
#' @name items
#' @rdname items
#' @export  
#' @param x Object to extract the items from
#' @return A character vector containing the names of the items
setGeneric("items", function(x) 
  standardGeneric("items") )

#' Export the item names for a TAMatrix.
#' @name items-TAMatrix
#' @rdname items-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases items-TAMatrix items,TAMatrix-method
#' @return Names of all items in TAMatrix.
#' 
setMethod("items",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(rownames(x@data))
          })


#' Give the sum of each column for the underlying matrix within an TAMatrix.
#' 
#' In the matrix underlying the TAMatrix the rows represent the items and the columns  represent 
#' the itemsets. Here the sums of all columns should be calculated that are the number of items
#' for each itesmet.
#' @name colSums-TAMatrix
#' @rdname colSums-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases colSums-TAMatrix colSums,TAMatrix-method
#' @return numeric vector containing the sum of each column of the TAMatrix
#' 
setMethod("colSums",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(colSums(x@data))
          })

#' Give the row Sums for the underlying matrix within an TAMatrix.
#' 
#' In the matrix underlying the TAMatrix the rows represent the items and the columns to represent 
#' the itemsets. Here the sums of each row should be calculated that are the number of occurences
#' of each item in the different transactions.
#' @name rowSums-TAMatrix
#' @rdname rowSums-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases rowSums-TAMatrix rowSums,TAMatrix-method
#' @return numeric vector containing the sum of each row of the TAMatrix
#' 
setMethod("rowSums",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(rowSums(x@data))
          })

#' Give the number of columns of underlying matrix in an TAMatrix. 
#' 
#' This number does represent the number of itemsets within that TAMatrix
#' @name ncol-TAMatrix
#' @rdname ncol-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases ncol-TAMatrix ncol,TAMatrix-method
#' @return number of columns / itemsets in the TAMatrix
#' 
setMethod("ncol",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(ncol(x@data))
          })

#' Give the number of rows of underlying matrix in an TAMatrix. 
#' 
#' This number does represent the number of items within that TAMatrix
#' @name nrow-TAMatrix
#' @rdname nrow-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases nrow-TAMatrix nrow,TAMatrix-method
#' @return number of rows / items in the TAMatrix
setMethod("nrow",  signature = signature(x = "TAMatrix"), 
          function(x){
            return(nrow(x@data))
          })


#' Subsetting of an TAMatrix
#' 
#' An TAMatrix does contain the matrix of all transactions as well as the dimensions of that 
#' matrix and the names of all items. Therefore, all these parts are logically connected and have
#' to be changed when the matrix is subsetted. 
#' @name subset-TAMatrix
#' @rdname subset-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of TAMatrix.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in TAMatrix
#' @aliases subset-TAMatrix subset,TAMatrix-method
#' @return subsetted TAMatrix
setMethod("[",  signature = signature(x = "TAMatrix"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if(!missing(i)){
              if (is.logical(i)){
                if(length(i) > nrow(x)){
                  stop(paste('Logical subscript of length', length(i), "too long for TAMatrix with", nrow(x), "rows"))
                }
              } else {
                if (is.numeric(i)){
                  if (any(! (i %in% 1:nrow(x)))){
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from TAMatrix with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if(!missing(j)){
              if (is.logical(j)){
                if(length(j) > ncol(x)){
                  stop(paste('Logical subscript of length', length(j), "too long for TAMatrix with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)){
                  if (any(! (j %in% 1:ncol(x)))){
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from TAMatrix with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
            
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

# ----------------------------------------------------------------------------- #
# ------------------ All methods for class FIMatrix --------------------------- #
# ----------------------------------------------------------------------------- #

#' Determine the number of items in a FIMatrix
#' 
#' The length function for the FIMatrix returns the number of frequent items
#' @name length-FIMatrix
#' @rdname length-FIMatrix
#' @param x Object of class FIMatrix
#' @aliases length-FIMatrix length,FIMatrix-method
#' @return Number of frequent itemsets within the FIMatrix
#' @export 

setMethod("length", "FIMatrix", function(x) {
  x@data@Dim[2]
})

#' Definition of show method for FIMatrix
#' 
#' The show function prints out the number of frequent itemsets in the FIMatrix
#' @name show-FIMatrix
#' @rdname show-FIMatrix
#' @export  
#' @param object Object of class FIMatrix
#' @aliases show-FIMatrix show,FIMatrix-method
#' @return Short message stating the number of frequent itemsets.
setMethod("show", "FIMatrix", function(object) {
  
  n <- length(object)
  cat("Found", n, "frequent itemset(s). Use print() to display\n")
  
})

#' Definition of show method for FIMatrix
#' 
#' The print function prints out all the frequent itemsets sorted by their support value.
#' @name print-FIMatrix
#' @rdname print-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @param descending Starting with the highest or lowest support value? 
#' @aliases print-FIMatrix print,FIMatrix-method
#' @return The frequent itemsets ordererd by their support
#' 
setMethod("print", signature(x = "FIMatrix"), function(x, descending = TRUE) {
  
  n <- x@data@Dim[1]
  output <- data.frame(items = rep(NA, n), support = rep(NA, n))
  
  for (i in 1:n) {
    output[i, 1] <- x@data@Dimnames[[1]][i]
    output[i, 2] <- x@support[i]
  }
  
  #order output by support before returning (default TRUE)
  output <- output[order(output$support, decreasing = descending), ]
  print(output)
  
})

#' Summary for FI-matrices.
#' 
#' The sumamary function gives general information about the frequent itemsets. 
#' @name summary-FIMatrix
#' @rdname summary-FIMatrix
#' @export  
#' @param object Object of class FIMatrix
#' @aliases summary-FIMatrix summary,FIMatrix-method
#' @return Summary information about the FImatrix
#' 
setMethod("summary", signature(object = "FIMatrix"), function(object) {
  
  n <- length(object)
  
  #Overview over frequent itemset matrix
  cat("\n")
  cat("Frequent itemsets in binary sparse matrix representation \n with", 
      nrow(object@data), "rows (items) and \n", 
      ncol(object@data), "columns (frequent itemsets)")
  cat("\n")
  
  #avoid unnecessary output when having less than 8 frequent itemsets
  if (nrow(object@data) < 8) {
    
    #top n most frequent items
    cat("\n")
    cat("Most frequent items: \n" )
    print(sort(rowSums(object@data), decreasing = T)[1:nrow(object@data)])
    cat("\n")
    
    #probability of observing top n items in an itemset
    cat("\n")
    cat("Observed frequency in frequent itemsets:\n")
    print(round(sort(rowSums(object@data), decreasing = T)[1:nrow(object@data)] / n, 4))
    cat("\n")
    
  } else {
    
    #top 8 most frequent items
    cat("\n")
    cat("Most frequent items: \n" )
    print(sort(rowSums(object@data), decreasing = T)[1:8])
    cat("\n")
    
    #probability of observing top 8 items in an itemset
    cat("\n")
    cat("Observed frequency in frequent itemsets:\n")
    print(round(sort(rowSums(object@data), decreasing = T)[1:8] / n, 4))
    cat("\n")
  }

  #distribution of frequent itemset lengths
  cat("\n")
  cat("Distribution of itemset length:\n")
  print(table(colSums(object@data)))
  cat("\n")
  
  #summary statistics on frequent itemset lengths
  print(summary(colSums(object@data)))
  cat("\n")
  
  #summary statistics on support measure
  cat("\n")
  cat("Summary of the support measure:\n")
  print(summary(object@support))
  cat("\n")
  
})


#' Plot an FI-matrices.
#' 
#' The plot function gives a scatter plot with the support on the y-axis and the itemset-length on
#' the x-axis
#' @name plot-FIMatrix
#' @rdname plot-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @param pch Size of points that is given to plot function.
#' @param col colour that is given to plot function.
#' @aliases plot-FIMatrix plot,FIMatrix-method
#' @return Scatter plot of Itemsize vs support.
#' 
setMethod("plot", signature(x = "FIMatrix"), function(x, pch = 1, col = "red") {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one itemset")
  } else {
  plot(colSums(x@data), x@support, 
       xlab = "Itemset length", ylab = "Support", 
       main = "Support distribution by itemset length", pch = pch, col = col, xaxt = "n")
    axis(1, at = seq(1, max(colSums(x@data)), by = 1), las=0)
  }
  
})


##ggplot2 implementation
# setMethod("qplot", signature(x = "FIMatrix"), function(x, col = "red", alpha = 0.1, type = c("hist", "scatter")) {
#   
#   if (missing(type)) {
#     stop("type missing: Please supply a type by specifiying either 'scatter' or 'hist'")
#   }
#   
#   #set up data frame for ggplot
#   df <- data.frame(data = colSums(x@data), support = x@support)
#   
#   if (type == "scatter") {
# 
#     ggplot(df, aes(data, support)) + 
#       geom_point(col = col, alpha = alpha) +
#       labs(x = "Itemset length", y = "support")
#   } else {
#     
#     ggplot(df, aes(df$data)) + 
#       geom_bar() +
#       labs(title = "Histogram of itemset lengths", x = "Itemset length") +
#       theme(plot.title = element_text(hjust = 0.5))
#   }
#   
# })

#' Plot an Histogram of itemset length for an FI-matrices.
#' 
#' The hist function gives a histogram of the lengths of the itemsets.
#' @name hist-FIMatrix
#' @rdname hist-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases hist-FIMatrix hist,FIMatrix-method
#' @return Histogram of Itemsize in FIMatrix.
#' 
setMethod("hist", "FIMatrix", function(x) {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one itemset")
  } else {
  hist(colSums(x@data), 
       main = "Histogram of frequent Itemsets", 
       xlab = "Itemset length", 
       col = "lightblue", xaxt = "n")
    axis(1, at = seq(1, max(colSums(x@data)), by = 1), las=0)
  }
})

#' Extracing the support of objects
#' 
#' Returns the support of the input objects
#' @name support
#' @rdname support
#' @export  
#' @param object Object to extract the support from
#' @return A numeric vector containing the support values.
setGeneric("support", function(object) {
  standardGeneric("support")
})

#' Extract the support of itemsets in class FIMatrix
#' @name support-FIMatrix
#' @rdname support-FIMatrix
#' @export  
#' @param object Object of class FIMatrix
#' @aliases support-FIMatrix support,FIMatrix-method
#' @return A numeric vector containing the support values of the itemsets in the FIMatrix.
#' 
setMethod("support", "FIMatrix", function(object) {
  return(object@support)
})


#' Pruning objects by metric.
#' 
#' With thes function one can eliminate parts of an object that do not fulfill certain threshholds.
#' Currently this function is implemented for the Classes FIMarix and Rules.
#' @name prune
#' @rdname prune
#' @export  
#' @param object Objected to be pruned.
#' @param ... metrics to prune by
#' @return Pruned object of same class as input.
setGeneric("prune", function(object, ...) {
  standardGeneric("prune")
})


#' Prune method for objects of class FIMatrix
#' 
#' With this function one can delete all itemsets from an FIMatrix that do not have minimal support.
#' @name prune-FIMatrix
#' @rdname prune-FIMatrix
#' @export  
#' @param object Object of class FIMatrix
#' @aliases prune-FIMatrix prune,FIMatrix-method
#' @param Support Minimal support the pruned FIMatrix or Rules should have.
#' @return Pruned object of class FIMatrix
#' 
setMethod("prune", "FIMatrix", function(object, Support) {
  
  # Error checking
  # Support should be numeric and within (0,1)
  if((!missing(Support)) && is.numeric(Support)){
    if (Support > 1 || Support < 0){
      stop("Supportort should be within (0,1). Pruning aborted.")
    }
  } else {
    if (!missing(Support)){
      stop('The Supportort specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  if (!missing(Support)){
    res <- object[,support(object) >= Support]
    return(res)
  } else {
    return(object) 
  }
})

#' Export the item names for a FIMatrix.
#' @name items-FIMatrix
#' @rdname items-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases items-FIMatrix items,FIMatrix-method
#' @return Names of all items in FIMatrix.
#' 
setMethod("items",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(rownames(x@data))
          })

#' Give the colSums for the underlying matrix within an FIMatrix.
#' 
#' In the matrix underlying the FIMatrix the rows represent the items and the columns to represent 
#' the itemsets. Here the sums of all columns should be calculated that are the number of items
#' for each itesmet.
#' @name colSums-FIMatrix
#' @rdname colSums-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases colSums-FIMatrix colSums,FIMatrix-method
#' @return numeric vector containing the sum of each column of the FIMatrix
#' 
setMethod("colSums",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(colSums(x@data))
          })

#' Give the row Sums for the underlying matrix within an FIMatrix.
#' 
#' In the matrix underlying the FIMatrix the rows represent the items and the columns to represent 
#' the itemsets. Here the sums of each row should be calculated that are the number of occurences
#' of each item.
#' @name rowSums-FIMatrix
#' @rdname rowSums-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases rowSums-FIMatrix rowSums,FIMatrix-method
#' @return numeric vector containing the sum of each row of the FIMatrix
#' 
setMethod("rowSums",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(rowSums(x@data))
          })

#' Give the number of columns of underlying matrix in an FIMatrix. 
#' 
#' This number does represent the number of itemsets within that FIMatrix
#' @name ncol-FIMatrix
#' @rdname ncol-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases ncol-FIMatrix ncol,FIMatrix-method
#' @return number of columns / itemsets in the FIMatrix
#' 
setMethod("ncol",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(ncol(x@data))
          })
#' Give the number of rows of underlying matrix in an FIMatrix. 
#' 
#' This number does represent the number of items within that FIMatrix
#' @name nrow-FIMatrix
#' @rdname nrow-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases nrow-FIMatrix nrow,FIMatrix-method
#' @return number of rows / items in the FIMatrix
setMethod("nrow",  signature = signature(x = "FIMatrix"), 
          function(x){
            return(nrow(x@data))
          })



#' Subsetting of an FIMatrix
#' 
#' An FImatrix does contain the matrix of itemsets as well as the a vector that contains the support
#' for each itemset. Therefore, both are logically connected and when a FIMatrix is subsetted column-
#' wise the supported vector is subsetted as well.
#' @name subset-FIMatrix
#' @rdname subset-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of FIMAtrix.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in FIMatrix
#' @aliases subset-FIMatrix subset,FIMatrix-method
#' @return subsetted FIMatrix
setMethod("[",  signature = signature(x = "FIMatrix"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if(!missing(i)){
              if (is.logical(i)){
                if(length(i) > nrow(x)){
                  stop(paste('Logical subscript of length', length(i), "too long for FIMatrix with", nrow(x), "rows"))
                }
              } else {
                if (is.numeric(i)){
                  if (any(! (i %in% 1:nrow(x)))){
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from FIMatrix with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if(!missing(j)){
              if (is.logical(j)){
                if(length(j) > ncol(x)){
                  stop(paste('Logical subscript of length', length(j), "too long for FIMatrix with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)){
                  if (any(! (j %in% 1:ncol(x)))){
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from FIMatrix with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
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



# ----------------------------------------------------------------------------- #
# -------------------- All methods for class Rules ---------------------------- #
# ----------------------------------------------------------------------------- #

#' Determine the number of rules in a Rules object
#' 
#' The length function for the Rules class returns the number of rules.
#' @name length-Rules
#' @rdname length-Rules
#' @param x Object of class Rules
#' @aliases length-Rules length,Rules-method
#' @return Number of Rules in x.
#' @export 
setMethod("length", "Rules", function(x) {
  x@lhs@Dim[2]
})


#' Definition of show method for Rules
#' 
#' The show function prints out the number of Rules
#' @name show-Rules
#' @rdname show-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases show-Rules show,Rules-method
#' @return Short message stating the number of Rules.
setMethod("show", "Rules", function(object) {
  
  n <- length(object)
  if (n > 0) {
    cat("Found", n, "rule(s). Use print() to display\n")
  } else {
    cat("Found no rules. Try lowering the support and/or confidence threshold.\n")
  }

})

#' Definition of show method for Rules
#' 
#' The print function prints out all the rules in the object sorted by specified matrices.
#' @name print-Rules
#' @rdname print-Rules
#' @export  
#' @param x Object of class Rules
#' @param maxNumConsequent The maximum length of consequents that the rules of the ouput should 
#' have. In Default all rules are shown.
#' @param order_by Specifiy up to four metrics out of support, confidence, lift, leverage by which
#' the given rules should be sorted. The first one used first and son on. 
#' @param decreasing Should the rules start with the smallest or highest values of the specified
#' metrics?
#' @aliases print-Rules print,Rules-method
#' @return The rules from the left hand and right hand side in the form of {It1, ... ItN} -> {ITK} 
#' in a data.frame. This data.frame does have columns lhs, rhs, unnamed, support and confidence.
#' 
setMethod("print", "Rules", function(x,maxNumConsequent = 1,
                                     order_by = NULL, decreasing = TRUE) {
  
  if (length(x) == 0) {
    return("Found no rules. Try lowering the support and/or confidence threshold.")
  } else {
    ExtractRules(x, maxNumConsequent = maxNumConsequent,
                 order_by = order_by, decreasing = decreasing)
  }
  
})


#' Summary funtion for Rules object
#' 
#' The summary for the rules object does give some general information on the quality of the rules.
#' @name summary-Rules
#' @rdname summary-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases summary-Rules summary,Rules-method
#' @return Summary information about the Rules
#' 
setMethod("summary", signature(object = "Rules"), function(object) {
  
  n <- length(object)
  quality <- data.frame(support = object@support, 
                        confidence = object@confidence,
                        lift = object@lift,
                        leverage = object@leverage)
  if (n > 0) {
    
    cat("Found", n, "rule(s). Use print() to display\n")
    cat("\n")
    
    #summary statistics on quality measures
    cat("\n")
    cat("Summary of quality measures:\n")
    print(summary(quality))
    cat("\n")
    
  } else {
    return("Found no rules. Try lowering the support and/or confidence threshold.")
  }

})

#' Plot an Rules object.
#' 
#' The plot function gives a scatter plot with the support on the x-axis, the confidence on the y-axis
#' and the lift as a color gradient. 
#' @name plot-Rules
#' @rdname plot-Rules
#' @export  
#' @param x Object of class Rules
#' @aliases plot-Rules plot,Rules-method
#' @return Scatter plot of support versus confidence, lift as a color gradient..
#' 
setMethod("plot", "Rules", function(x) {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one rule")
  } else {
    
    #color gradient function
    colfunc <- colorRampPalette(c("lightblue", "blue"))
    
    #needed for ordering
    plot.df <- data.frame(support = x@support, 
                          confidence = x@confidence,
                          lift = x@lift)
    
    #ordering needed for color gradient
    plot.df <- plot.df[order(plot.df$lift), ]
    
    #main scatterplot 
    layout(matrix(1:2, ncol = 2), width = c(2, 1), height = c(1, 1))
    plot(plot.df$support, 
         plot.df$confidence, 
         xlab = "Support", ylab = "Confidence", 
         pch = 20, col = colfunc(length(x)))
    
    #gradient legend
    legend.raster <- as.raster(matrix(rev(colfunc(length(x)))), ncol = 1)
    plot(c(0, 2), 
         c(0, round(max(x@lift), 2)), 
         type = "n", axes = F, xlab = "", ylab = "", main = "Lift", adj = 0.225)
    text(x = 1.5, 
         y = seq(0, max(x@lift), l = 3), 
         labels = seq(round(min(x@lift), 2), round(max(x@lift), 2), l = 3))
    rasterImage(legend.raster, 0, 0, 1, round(max(x@lift), 2))
    
  }

})

# #ggplot2 implementation
# setMethod("qplot", "Rules", function(x) {
#   
#   quality.df <- data.frame(support = x@support, 
#                            confidence = x@confidence,
#                            lift = x@lift,
#                            leverage = x@leverage)
#   
#   ggplot(quality.df, aes(x=support, y=confidence, color=lift)) + 
#     geom_point() + 
#     scale_color_gradient(low="lightblue", high="blue")
#   
# })


#' Extract confidence from object
#' @name confidence
#' @rdname confidence
#' @export
#' @param object Object of Class Rules
#' @return Vector of confidence values from all entities of the objects.

setGeneric("confidence", function(object) {
  standardGeneric("confidence")
})


#' Extract confidence of all rules within a Rules object.
#' @name confidence-Rules
#' @rdname confidence-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases confidence-Rules confidence,Rules-method
#' @return Vector of confidence values from all Rules in x.
#' 
setMethod("confidence", "Rules", function(object) {
  object@confidence
})

#' Extract lift from object
#' @name lift
#' @rdname lift
#' @export
#' @param object Object of Class Rules
#' @return Vector of lift values from all entities of the objects.

setGeneric("lift", function(object) {
  standardGeneric("lift")
})

#' Extract lift of all rules within a Rules object.
#' @name lift-Rules
#' @rdname lift-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases lift-Rules lift,Rules-method
#' @return Vector of lift values from all Rules in x.
#' 
setMethod("lift", "Rules", function(object) {
  object@lift
})

#' Extract leverage from object
#' @name leverage
#' @rdname leverage
#' @export
#' @param object Object of Class Rules
#' @return Vector of leverage values from all entities of the objects.

setGeneric("leverage", function(object) {
  standardGeneric("leverage")
})

#' Extract leverage of all rules within a Rules object.
#' @name leverage-Rules
#' @rdname leverage-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases leverage-Rules leverage,Rules-method
#' @return Vector of leverage values from all Rules in x.
setMethod("leverage", "Rules", function(object) {
  object@leverage
})

#' Extract the support of itemsets in class Rules
#' @name support-Rules
#' @rdname support-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases support-Rules support,Rules-method
#' @return A numeric vector containing the support values of Rules.
setMethod("support", "Rules", function(object) {
  object@support
})

#' Extract FIMatrix object from class
#' @name extract
#' @rdname extract
#' @export
#' @param object Object of Class Rules
#' @return Object of Class FIMatrix

setGeneric("extract", valueClass = "FIMatrix", function(object) {
  standardGeneric("extract")
})

#' Extract the FIMatrix from a rules object
#' @name extract-Rules
#' @rdname extract-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases extract-Rules extract,Rules-method
#' @return FIMatrix containing the frequent itemsets based on which the rules where calculated.
setMethod("extract", "Rules", function(object) {
  object@FrequentItemsets
})


#' Prune method for objects of class Rules
#' 
#' With this function one can delete all itemsets from an Rules that do not have minimal support,
#' confidence lift or leverage.
#' @name prune-Rules
#' @rdname prune-Rules
#' @export  
#' @aliases prune-Rules prune,Rules-method
#' @param object Object of class Rules
#' @param Support Minimal support the output rules should have.
#' @param Confidence Minimal confidence the output rules should have.
#' @param Lift Minimal Lift the output rules should have.
#' @param Leverage Minimal Leverage the output rules should have.
#' @param inv_Lift Pruning based on minimal or maximal lift?
#' @param inv_lev Pruning based on minimal or maximal leverage?
#' @return Pruned object of class Rules

setMethod("prune", "Rules", function(object, Support, Confidence, Lift, Leverage,
                                     inv_Lift = FALSE, inv_lev = FALSE) {
  
  # Error checking
  # Support should be numeric and within (0,1)
  if((!missing(Support)) && is.numeric(Support)){
    if (Support > 1 || Support < 0){
      stop("Supportort should be within (0,1). Pruning aborted.")
    }
  } else {
    if(!missing(Support)){
      stop('The Supportort specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  # Confidenceidence should be numeric and within (0,1)
  if((!missing(Confidence)) && is.numeric(Confidence)){
    if (Confidence > 1 || Confidence < 0){
      stop("Confidenceidence should be within (0,1). Pruning aborted.")
    }
  } else {
    if(!missing(Confidence)){
      stop('The Confidenceidence specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  # Lift should be numeric
  if ((!missing(Lift)) && !is.numeric(Lift)){
    stop("Lift should be numeric. Pruning aborted")
  }
  
  # Leverage should be numeric
  if ((!missing(Leverage)) &&!is.numeric(Leverage)){
    stop("Leverage should be numeric. Pruning aborted")
  }
  
  # If non of the paramters is specified all colums / itemsets should be returned.
  select <- rep(TRUE, ncol(object))
  
  # 
  if(!missing(Support)){
    select <- select & support(object) >= Support
  }
  if(!missing(Confidence)){
    select <- select & confidence(object) >= Confidence
  }
  if(!missing(Lift)){
    if(!inv_Lift){
      select <- select & lift(object) >= Lift
    } else {
      select <- select & lift(object) <= Lift
    }
    
    
  }
  if(!missing(Leverage)){
    if(!inv_lev){
      select <- select & leverage(object) >= Leverage
    } else {
      select <- select & leverage(object) <= Leverage
    }
  }
  
  res <- object[,select]
  
  return(res)
})


#' Give the sum of each row for the for either the rhs or lhs of a rule.
#' 
#' Underlying a rules there is a right-hand side and a left-hand side. Both are stored as matrices. 
#' With this function one can calculated the sum of each row, that is the respective number
#' of occurences of each item in all transactions for either the left-hand side or the right-hand side.
#' @name rowSums-Rules
#' @rdname rowSums-Rules
#' @export  
#' @param x Object of class Rules
#' @param lhs if true the sum of each rwo of the left-hand side are calculated, else the sum of 
#' each row of the right-hand side.
#' @aliases rowSums-Rules rowSums,Rules-method
#' @return numeric vector containing the sum of each row of either the rhs or the lhs.
setMethod("rowSums",  signature = signature(x = "Rules"), 
          function(x, lhs = TRUE){
            if (lhs){
              return(rowSums(x@lhs))
            } else {
              return(rowSums(x@rhs))
            }
          })

#' Give the sum of each column for the for either the rhs or lhs of a rule.
#' 
#' Underlying a rules there is a right-hand side and a left-hand side. Both are stored as matrices. 
#' With this function one can calculated the sum of each column, that is the respective number
#' of items within each itemset for either the left-hand side or the right-hand side.
#' @name colSums-Rules
#' @rdname colSums-Rules
#' @export  
#' @param x Object of class Rules
#' @param lhs if true the sum of each column of the left-hand side are calculated, else the sum of 
#' each column of the right-hand side.
#' @aliases colSums-Rules colSums,Rules-method
#' @return numeric vector containing the sum of each column of either the rhs or the lhs.
setMethod("colSums",  signature = signature(x = "Rules"), 
          function(x, lhs = TRUE){
            if (lhs){
              return(colSums(x@lhs))
            } else {
              return(colSums(x@rhs))
            }
          })

#' Export the item names for a Rules object.
#' @name items-Rules
#' @rdname items-Rules
#' @export  
#' @param x Object of class Rules
#' @aliases items-Rules items,Rules-method
#' @return Vector containing the names of all items in Rules.
#' 
setMethod("items",  signature = signature(x = "Rules"), 
          function(x){
            return(rownames(x@lhs))
          })


#' Give the number of columns of underlying matrix in an Rules object. 
#' 
#' Although a Rules object does have left-hand side and a right hand-side the number of columns for
#' both does represent the number rules and therefore should be the same for both sides. This functions
#' simply uses the left-hand sides as proxy.
#' @name ncol-Rules
#' @rdname ncol-Rules
#' @export  
#' @param x Object of class Rules
#' @aliases ncol-Rules ncol,Rules-method
#' @return number of columns / Rules in the Rules object.
#' 
setMethod("ncol",  signature = signature(x = "Rules"), 
          function(x){
            return(ncol(x@lhs))
          })

#' Give the number of rows of underlying matrix in an Rules object. 
#' 
#' Although a Rules object does have left-hand side and a right hand-side the number of rows for
#' both does represent the number items and therefore should be the same for both sides. This functions
#' simply uses the left-hand sides as proxy.
#' @name nrow-Rules
#' @rdname nrow-Rules
#' @export  
#' @param x Object of class Rules
#' @aliases nrow-Rules nrow,Rules-method
#' @return number of rows / total number of possible items in the Rules object.
#' 
setMethod("nrow",  signature = signature(x = "Rules"), 
          function(x){
            return(nrow(x@lhs))
          })

#' Subsetting of an Rules object.
#' 
#' An Rules does contain the matrix of itemsets as well as the a vectors that contains the support,
#' confidence, lift and leverage for all rules.Therefore, both are logically connected and 
#' when a Rules is subsetted column- wise the other vectors are subsetted as well.
#' @name subset-Rules
#' @rdname subset-Rules
#' @export  
#' @param x Object of class Rules
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of Rules.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in Rules
#' @aliases subset-Rules subset,Rules-method
#' @return subsetted Rules object.
setMethod("[",  signature = signature(x = "Rules"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if(!missing(i)){
              if (is.logical(i)){
                if(length(i) > nrow(x)){
                  stop(paste('Logical subscript of length', length(i), "too long for Rules with", nrow(x), "rows"))
                }
              } else {
                if (is.numeric(i)){
                  if (any(! (i %in% 1:nrow(x)))){
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from Rules with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if(!missing(j)){
              if (is.logical(j)){
                if(length(j) > ncol(x)){
                  stop(paste('Logical subscript of length', length(j), "too long for Rules with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)){
                  if (any(! (j %in% 1:ncol(x)))){
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from Rules with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
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




