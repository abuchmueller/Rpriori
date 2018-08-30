# ------------------------------------------------------------------------------------------------ #
# ----------------------------- All methods for the classes. ------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------ #
# ---------------------------- All methods for class TAMATRIX ------------------------------------ #
# ------------------------------------------------------------------------------------------------ #

#' Determine the number of items in a TAMatrix
#' 
#' The length function for the TAMatrix returns the number of items in that TAMatrix.
#' @name length-TAMatrix
#' @rdname length-TAMatrix
#' @param x Object of class TAMatrix
#' @aliases length-TAMatrix length,TAMatrix-method
#' @return Number of  items within the TAMatrix
#' @export 
#' @importFrom grDevices as.raster colorRampPalette
#' @importFrom graphics layout rasterImage text axis
#' @import methods
#' @include allClasses.R
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

#' Plot an TAMatrix object.
#' 
#' The plot function for TA Matrix does give a histgram of the itemset length.
#' @name plot-TAMatrix
#' @rdname plot-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases plot-TAMatrix plot,TAMatrix-method
#' @return histogram of the length of the itemsets within the TA Matrix.
#' @importFrom utils data
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

#' ggplot plot methods for Rules, TAMatrix and FIMatrix
#' @param x object of class Rules, TAMatrix or FIMatrix
#' @param ... Further information that should be passed down to plot function
#' @return Plot
#' @export
#' @importFrom ggplot2 aes element_text geom_bar geom_point ggplot labs scale_color_gradient theme scale_x_continuous
setGeneric("qplot", function(x,... )  standardGeneric("qplot"))

#' Plot an TAMatrix object with ggplot.
#' 
#' The qplot function for TA Matrix does give a histgram of the itemset length.
#' @name qplot-TAMatrix
#' @rdname qplot-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases qplot-TAMatrix qplot,TAMatrix-method
#' @return histogram of the length of the itemsets within the TA Matrix.
#' @export
#' @importFrom arules items support
setMethod("qplot", signature(x = "TAMatrix"), function(x) {

  #dataframe needed for ggplot
  df <- data.frame(itemset_length = colSums(x@data))

  ggplot(df, aes(df$itemset_length)) +
    geom_bar() +
    labs(title = "Histogram of itemset lengths", x = "Itemset length") +
    theme(plot.title = element_text(hjust = 0.5))
})

#' Export the item names for a TAMatrix.
#' @name items-TAMatrix
#' @rdname items-TAMatrix
#' @export  
#' @param x Object of class TAMatrix
#' @aliases items-TAMatrix items,TAMatrix-method
#' @return Names of all items in TAMatrix.
setMethod("items",  signature = signature(x = "TAMatrix"), 
          function(x) {
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
setMethod("colSums",  signature = signature(x = "TAMatrix"), 
          function(x) {
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
setMethod("rowSums",  signature = signature(x = "TAMatrix"), 
          function(x) {
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
setMethod("ncol",  signature = signature(x = "TAMatrix"), 
          function(x) {
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
          function(x) {
            return(nrow(x@data))
          })

#' Subsetting  of TAMatrix, FIMatrix and Rules class. 
#' @name select
#' @rdname select
#' @export  
#' @param x Object to subset
#' @param i Either rows represented by their row number or logical vector of length
#' number of rows
#' @param j Either columns represented by their column number or logical vector of length
#' number of columns
#' @return A character vector containing the names of the items
setGeneric("select", function(x, i, j) standardGeneric("select"))

#' Subsetting of an TAMatrix
#' 
#' An TAMatrix does contain the matrix of all transactions as well as the dimensions of that 
#' matrix and the names of all items. Therefore, all these parts are logically connected and have
#' to be changed when the matrix is subsetted. 
#' @name select-TAMatrix
#' @rdname select-TAMatrix
#' @param x Object of class TAMatrix
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of TAMatrix. If missing or NULL all rows are selected.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in TAMatrix. If missing or NULL all columns are selected.
#' @aliases select-TAMatrix select,TAMatrix-method
#' @return subsetted TAMatrix
setMethod("select",  signature = signature(x = "TAMatrix"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if (!(missing(i) || is.null(i))) {
              if (is.logical(i)) {
                if (length(i) > nrow(x)) {
                  stop(paste('Logical subscript of length',
                             length(i), "too long for TAMatrix with", nrow(x), "rows"))
                }
              } else {
                if (is.numeric(i)) {
                  if (any(!(i %in% 1:nrow(x)))) {
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from TAMatrix with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if (!(missing(j) || is.null(j))) {
              if (is.logical(j)) {
                if (length(j) > ncol(x)) {
                  stop(paste('Logical subscript of length', length(j), "too long for TAMatrix with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)) {
                  if (any(!(j %in% 1:ncol(x)))) {
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from TAMatrix with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
            
            # If i or j are missing, all rows / columns should be selected.
            if (missing(i) || is.null(i)) {
              
              if (is.logical(j)) {
                j <- which(j)
              }
              
              return(new('TAMatrix',
                         data = x@data[, j, drop = FALSE],
                         dim = c(nrow(x@data), length(j)),
                         items = x@items))
            }
            
            if (missing(j) || is.null(j)) {
              
              if (is.logical(i)) {
                i <- which(i)
              }
              
              return(new('TAMatrix',
                         data = x@data[i, , drop = FALSE],
                         dim = c(length(i), ncol(x@data)),
                         items = x@items[i, drop = FALSE]))
            }
            
            if ((missing(i) || is.null(i)) && (missing(j) || is.null(j))) {
              return(x)
            }
            
            # If i or j is logical than make it to an integer of the true positions
            if (is.logical(i)) {
              i <- which(i)
            }
            
            if (is.logical(j)) {
              j <- which(j)
            }
            
            return(new('TAMatrix',
                       data = x@data[i, j, drop = FALSE],
                       dim = c(length(i), length(j)),
                       items = x@items[i, drop = FALSE]))
          })

# ------------------------------------------------------------------------------------------------ #
# -------------------------------- All methods for class FIMatrix -------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Determine the number of items in a FIMatrix
#' 
#' The length function for the FIMatrix returns the number of frequent items.
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
#' The show function prints out the number of frequent itemsets in the FIMatrix.
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
setMethod("plot", signature(x = "FIMatrix"), function(x, pch = 1, col = "red") {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one itemset")
  } else {
  plot(colSums(x@data), x@support, 
       xlab = "Itemset length", ylab = "Support", 
       main = "Support distribution by itemset length", pch = pch, col = col, xaxt = "n")
    axis(1, at = seq(1, max(colSums(x@data)), by = 1), las = 0)
  }
})

#' Plot an FI-matrices with ggplot
#' 
#' The qplot function can do a scatter plot with the support on the y-axis and the itemset-length on
#' the x-axis or a histogram of itemset lengths
#' @name qplot-FIMatrix
#' @rdname qplot-FIMatrix
#' @param x Object of class FIMatrix
#' @param col colour of data points (only scatter plot) per default "red"
#' @param alpha alpha value for scatter plot
#' @param type character string "hist" or "scatter" depending on wether you want a histogram or scatter plot
#' @aliases qplot-FIMatrix qplot,FIMatrix-method
#' @return Scatter plot of itemset length against support or histogram of itemset lengths
#' @export
setMethod("qplot", signature(x = "FIMatrix"),
          function(x, col = "red", alpha = 0.1, type = c("hist", "scatter")) {

  if (missing(type)) {
    type == "scatter"
  }

  #set up data frame for ggplot
  type = type
  df <- data.frame(data = colSums(x@data), support = x@support)

  if (type == "scatter") {
    
    # Make a scatter plot
    ggplot(df, aes(data, support)) +
      geom_point(col = col, alpha = alpha) +
      labs(x = "Itemset length", y = "support") +
      scale_x_continuous(breaks = seq(1, max(colSums(x@data)), by = 1))
  } else if (type == "hist") {
    
    # Make a histogram.
    ggplot(df, aes(df$data)) +
      geom_bar() +
      labs(title = "Histogram of itemset lengths", x = "Itemset length") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    stop("Please supply a valid 'type' argument 'hist' or 'scatter' ")
  }
})

#' Plot an Histogram of itemset length for an FI-matrices.
#' 
#' The hist function gives a histogram of the lengths of the itemsets.
#' @name hist-FIMatrix
#' @rdname hist-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases hist-FIMatrix hist,FIMatrix-method
#' @return Histogram of Itemsize in FIMatrix.
setMethod("hist", "FIMatrix", function(x) {
  
  if (length(x) <= 0) {
    stop("Object must contain at least one itemset")
  } else {
  hist(colSums(x@data), 
       main = "Histogram of frequent Itemsets", 
       xlab = "Itemset length", 
       col = "lightblue", xaxt = "n")
    axis(1, at = seq(1, max(colSums(x@data)), by = 1), las = 0)
  }
})

#' Extract the support of itemsets in class FIMatrix
#' @name support-FIMatrix
#' @rdname support-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases support-FIMatrix support,FIMatrix-method
#' @return A numeric vector containing the support values of the itemsets in the FIMatrix.
setMethod("support", "FIMatrix", function(x) {
  return(x@support)
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
setGeneric("prune", function(object, ...) standardGeneric("prune"))

#' Prune method for objects of class FIMatrix
#' 
#' With this function one can delete all itemsets from an FIMatrix that do not have minimal support.
#' @name prune-FIMatrix
#' @rdname prune-FIMatrix
#' @export  
#' @param object Object of class FIMatrix.
#' @aliases prune-FIMatrix prune,FIMatrix-method
#' @param Support Minimal support the pruned FIMatrix should have.
#' @return Pruned object of class FIMatrix
setMethod("prune", "FIMatrix", function(object, Support) {
  
  # Error checking
  # Support should be numeric and within (0,1)
  if ((!missing(Support)) && is.numeric(Support)) {
    if (Support > 1 || Support < 0) {
      stop("Supportort should be within (0,1). Pruning aborted.")
    }
  } else {
    if (!missing(Support)) {
      stop('The Supportort specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  if (!missing(Support)) {
    res <- select(object,support(object) >= Support, NULL)
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
setMethod("items",  signature = signature(x = "FIMatrix"), 
          function(x) {
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
setMethod("colSums",  signature = signature(x = "FIMatrix"), 
          function(x) {
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
setMethod("rowSums",  signature = signature(x = "FIMatrix"), 
          function(x) {
            return(rowSums(x@data))
          })

#' Give the number of columns of the underlying matrix in an FIMatrix. 
#' 
#' This number does represent the number of itemsets within that FIMatrix
#' @name ncol-FIMatrix
#' @rdname ncol-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases ncol-FIMatrix ncol,FIMatrix-method
#' @return number of columns / itemsets in the FIMatrix
setMethod("ncol",  signature = signature(x = "FIMatrix"), 
          function(x) {
            return(ncol(x@data))
          })

#' Give the number of rows of the underlying matrix in an FIMatrix. 
#' 
#' This number does represent the number of items within that FIMatrix
#' @name nrow-FIMatrix
#' @rdname nrow-FIMatrix
#' @export  
#' @param x Object of class FIMatrix
#' @aliases nrow-FIMatrix nrow,FIMatrix-method
#' @return number of rows / items in the FIMatrix
setMethod("nrow",  signature = signature(x = "FIMatrix"), 
          function(x) {
            return(nrow(x@data))
          })

#' Subsetting of an FIMatrix
#' 
#' An FImatrix does contain the matrix of itemsets as well as the a vector that contains the support
#' for each itemset. Therefore, both are logically connected and when a FIMatrix is subsetted column-
#' wise the supported vector has to be subsetted as well.
#' @name select-FIMatrix
#' @rdname select-FIMatrix
#' @export  
#' @param x Object of class FIMatrix.
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of FIMAtrix. If missing or NULL all rows are selected.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in FIMatrix. If missing or NULL all columns are selected.
#' @aliases select-FIMatrix select,FIMatrix-method
#' @return subsetted FIMatrix
setMethod("select",  signature = signature(x = "FIMatrix"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if (!(missing(i) || is.null(i))) { 
              if (is.logical(i)) { 
                if (length(i) > nrow(x)) { 
                  stop(paste('Logical subscript of length', length(i), "too long for FIMatrix with", nrow(x), "rows"))
                }
              } else { 
                if (is.numeric(i)) {  
                  if (any(!(i %in% 1:nrow(x)))) { 
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from FIMatrix with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if (!(missing(j) || is.null(j))) {
              if (is.logical(j)) {
                if (length(j) > ncol(x)) {
                  stop(paste('Logical subscript of length', length(j), "too long for FIMatrix with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)) {
                  if (any(!(j %in% 1:ncol(x)))) {
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from FIMatrix with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
            # If the matrix does not have row or columns return an empty matrix
            if (nrow(x@data) == 0 || ncol(x@data) == 0) {
              return(new('FIMatrix',
                         data = x@data[0, 0, drop = FALSE],
                         support = x@support[0, drop = FALSE]))
            }
            
            # If i is missing use all rows of the input 
            if (missing(i) || is.null(i)) {
              i <- 1:nrow(x@data)
            }
            
            # If j is missing use all columns of the input 
            if (missing(j) || is.null(j)) {
              j <- 1:ncol(x@data)
            }
            
            # if i, j is logical replace it by the positions of the true values
            if (is.logical(i)) {
              i <- which(i)
              i <- as.numeric(i)
            }
            
            if (is.logical(j)) {
              j <- which(j)
              j <- as.numeric(j)
            }
            
            if (length(i) == 0 || length(j) == 0) {
              return(new('FIMatrix',
                         data = x@data[0, 0, drop = FALSE],
                         support = x@support[0, drop = FALSE]))
            }
            
            
            return(new('FIMatrix',
                       data = x@data[i, j, drop = FALSE],
                       support = x@support[j, drop = FALSE]))
            
          })

# ------------------------------------------------------------------------------------------------ #
# ------------------------------ All methods for class Rules ------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

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
#' @return Scatter plot of support versus confidence with lift as a color gradient
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
    layout(matrix(1:2, ncol = 2), widths = c(2, 1), heights = c(1, 1))
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

#' Plot an Rules object wiht ggplot2
#' 
#' The plot function gives a scatter plot with the support on the x-axis, the confidence on the y-axis
#' and the lift as a color gradient. 
#' @name qplot-Rules
#' @rdname qplot-Rules
#' @export  
#' @param x Object of class Rules
#' @aliases qplot-Rules qplot,Rules-method
#' @return Scatter qplot of support versus confidence, lift as a color gradient..
setMethod("qplot", "Rules", function(x) {

  quality.df <- data.frame(support = x@support,
                           confidence = x@confidence,
                           lift = x@lift,
                           leverage = x@leverage)

  ggplot(quality.df, aes(x = support, y = confidence, color = lift)) +
    geom_point() +
    scale_color_gradient(low = "lightblue", high = "blue")
})

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
setMethod("confidence", "Rules", function(object) {
  object@confidence
})

#' Extract lift from object
#' @name lift
#' @rdname lift
#' @export
#' @param object Object of Class Rules
#' @return Vector of lift values from all entities of the objects.
setGeneric("lift", function(object) standardGeneric("lift"))

#' Extract lift of all rules within a Rules object.
#' @name lift-Rules
#' @rdname lift-Rules
#' @export  
#' @param object Object of class Rules
#' @aliases lift-Rules lift,Rules-method
#' @return Vector of lift values from all Rules in x.
setMethod("lift", "Rules", function(object) {
  object@lift
})

#' Extract leverage from object
#' @name leverage
#' @rdname leverage
#' @export
#' @param object Object of Class Rules
#' @return Vector of leverage values from all entities of the objects.
setGeneric("leverage", function(object) standardGeneric("leverage"))

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
#' @param x Object of class Rules
#' @aliases support-Rules support,Rules-method
#' @return A numeric vector containing the support values of Rules.
setMethod("support", "Rules", function(x) {
  x@support
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
  if ((!missing(Support)) && is.numeric(Support)) {
    if (Support > 1 || Support < 0) {
      stop("Supportort should be within (0,1). Pruning aborted.")
    }
  } else {
    if (!missing(Support)) {
      stop('The Supportort specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  # Confidenceidence should be numeric and within (0,1)
  if ((!missing(Confidence)) && is.numeric(Confidence)) {
    if (Confidence > 1 || Confidence < 0) {
      stop("Confidenceidence should be within (0,1). Pruning aborted.")
    }
  } else {
    if (!missing(Confidence)) {
      stop('The Confidenceidence specified in Support should be numeric!. Pruning aborted.')
    }
  }
  
  # Lift should be numeric
  if ((!missing(Lift)) && !is.numeric(Lift)) {
    stop("Lift should be numeric. Pruning aborted")
  }
  
  # Leverage should be numeric
  if ((!missing(Leverage)) && !is.numeric(Leverage)) {
    stop("Leverage should be numeric. Pruning aborted")
  }
  
  # If non of the paramters is specified all colums / itemsets should be returned.
  selection <- rep(TRUE, ncol(object))
  
  if (!missing(Support)) {
    selection <- selection & support(object) >= Support
  }
  if (!missing(Confidence)) {
    selection <- selection & confidence(object) >= Confidence
  }
  if (!missing(Lift)) {
    if (!inv_Lift) {
      selection <- selection & lift(object) >= Lift
    } else {
      selection <- selection & lift(object) <= Lift
    }
    
    
  }
  if (!missing(Leverage)) {
    if (!inv_lev) {
      selection <- selection & leverage(object) >= Leverage
    } else {
      selection <- selection & leverage(object) <= Leverage
    }
  }
  
  res <- select(object,NULL, selection)
  
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
          function(x, lhs = TRUE) {
            if (lhs) {
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
          function(x, lhs = TRUE) {
            if (lhs) {
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
setMethod("items",  signature = signature(x = "Rules"), 
          function(x) {
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
setMethod("ncol",  signature = signature(x = "Rules"), 
          function(x) {
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
setMethod("nrow",  signature = signature(x = "Rules"), 
          function(x) {
            return(nrow(x@lhs))
          })

#' Subsetting of an Rules object.
#' 
#' An Rules does contain the matrix of itemsets as well as the a vectors that contains the support,
#' confidence, lift and leverage for all rules.Therefore, both are logically connected and 
#' when a Rules is subsetted column- wise the other vectors are subsetted as well.
#' @name select-Rules
#' @rdname select-Rules
#' @export  
#' @param x Object of class Rules
#' @param i Either the rows represented by their row number or a logical vector of length number of 
#' row of Rules. If i is missing or NULL all rows are selected.
#' @param j Either the columns represented by their columns numbers or logical vector of length 
#' number of columns in Rules. If j is missing or NULL all columns are selected.
#' @aliases select-Rules select,Rules-method
#' @return subsetted Rules object.
setMethod("select",  signature = signature(x = "Rules"), 
          function(x, i, j) {
            
            # Make some sanity checks on i, j.
            if (!(missing(i) || is.null(i))) {
              if (is.logical(i)) {
                if (length(i) > nrow(x)) {
                  stop(paste('Logical subscript of length', length(i), "too long for Rules with", nrow(x), "rows"))
                }
              } else {
                if (is.numeric(i)) {
                  if (any(!(i %in% 1:nrow(x)))) {
                    stop(paste("Subscript is too long. (", paste(i[!i %in% 1:nrow(x)], collapse = ', '),
                               ") cannot be subsetted from Rules with ", nrow(x), ' rows', sep = ''))
                  }
                }
              }
            }
            
            if (!(missing(j) || is.null(j))) {
              if (is.logical(j)) {
                if (length(j) > ncol(x)) {
                  stop(paste('Logical subscript of length', length(j), "too long for Rules with",
                             ncol(x), "columns"))
                }
              } else {
                if (is.numeric(j)) {
                  if (any(!(j %in% 1:ncol(x)))) {
                    stop(paste("Subscript is too long. (", paste(j[!j %in% 1:ncol(x)], collapse = ', '),
                               ") cannot be subsetted from Rules with ", ncol(x), ' columns', sep = ''))
                  }
                }
              }
            }
            
            # If the matrix does not have row or columns return an empty matrix
            if (nrow(x@lhs) == 0 || ncol(x@lhs) == 0 || nrow(x@rhs) == 0 || ncol(x@rhs) == 0) {
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
            if (missing(i) || is.null(i)) {
              i <- 1:nrow(x@lhs)
            }
            
            # If j is missing use all columns of the input 
            if (missing(j) || is.null(j)) {
              j <- 1:ncol(x@lhs)
            }
            
            # if i, j is logical replace it by the positions of the true values
            if (is.logical(i)) {
              i <- which(i)
              i <- as.numeric(i)
            }
            
            if (is.logical(j)) {
              j <- which(j)
              j <- as.numeric(j)
            }
            
            if (length(i) == 0 || length(j) == 0) {
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