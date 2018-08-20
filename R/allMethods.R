#### This file stores printing all methods for our classes (TAMatrix, FIMatrix & Rules) ####
#############################################################################################

#' @include allClasses.R

## TAMatrix ##
##############

setMethod("length", "TAMatrix", function(x) {
  x@dim[1]
})

setMethod("print", "TAMatrix", function(x) {
  
  #collect all itemnames, print them by their frequency in descending order
  print(sort(rowSums(x@data), decreasing = TRUE))
  
})

setMethod("show", "TAMatrix", function(object) {
  
  n <- length(object)
  cat(n, "items. Use the print function to display\n")
  
})

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
  
  #determine maximum length of itemsets to obtain optimal number breakpoints
  max.itemlength <- max(colSums(x@data))
  
  hist(colSums(x@data), 
       breaks = max.itemlength + 1, 
       main = "Distribution of itemset lengths", xlab = "Itemset length", 
       col = "lightblue")
  
})

## FrequentItems ##
###################

setMethod("length", "FIMatrix", function(x) {
  x@data@Dim[2]
})

setMethod("show", "FIMatrix", function(object) {
  
  n <- length(object)
  cat(n, "frequent itemsets. Use the print function to display\n")
  
})

# combines the elements of both lists, i.e., a frequent item and it's corressponding support into a data frame and displays side by side
setMethod("print", signature(x = "FIMatrix"), function(x) {
  
  n <- x@data@Dim[1]
  output <- data.frame(items = rep(NA, n), support = rep(NA, n))
  
  for (i in 1:n) {
    output[i, 1] <- x@data@Dimnames[[1]][i]
    output[i, 2] <- x@support[i]
  }
  
  print(output)
  
})


setMethod("summary", signature(object = "FIMatrix"), function(object) {
  
  #Overview over frequent itemset matrix
  cat("\n")
  cat("Frequent itemsets in binary sparse matrix representation \n with", 
      nrow(object@data), "rows (items) and \n", 
      ncol(object@data), "columns (frequent itemsets)")
  cat("\n")
  
  #top 8 most frequent items
  cat("\n")
  cat("Most frequent items: \n" )
  print(sort(rowSums(object@data), decreasing = T)[1:8])
  cat("\n")
  
  #probability of observing top 8 items in an itemset
  cat("\n")
  cat("Observed frequency in frequent itemsets:\n")
  print(round(sort(rowSums(fitems@data), decreasing = T)[1:8] / length(object), 4))
  cat("\n")
  
  #distribution of frequent itemset lengths
  cat("\n")
  cat("Distribution of itemset length:\n")
  print(table(colSums(object@data)))
  cat("\n")
  
  #summary statistics on frequent itemset lengths
  print(summary(colSums(object@data)))
  cat("\n")
  
})

#Plot itemsets against support
setMethod("plot", signature(x = "FIMatrix"), function(x, pch = 1, col = 1) {
  
  plot(colSums(x@data), x@support, 
       xlab = "Itemset length", ylab = "Support", 
       main = "Support distribution by itemset length", pch = pch, col = col)
  
})

setMethod("hist", "FIMatrix", function(x) {
  hist(colSums(x@data), main = "Histogram of frequent Itemsets", xlab = "Itemset length", col = "lightblue")
})

## Rules ## 
###########

#display the rules in a way that a human can read them easily: if lhs is purchased => rhs is frequently purchased, too (+support & confidence).
setMethod("length", "Rules", function(x) {
  x@lhs@Dim[2]
})

setMethod("show", "Rules", function(object) {
  
  n <- length(object)
  cat(n, "rules. Use the print function to display\n")
  
})

#display the rules in a way that a human can read them easily: 
#if lhs is purchased => rhs is frequently purchased, too (+support & confidence).
setMethod("print", "Rules", function(x) {
  ExtractRules(x, maxNumConsequent = 1)
})


