#### This file stores printing all methods for our classes (TAMatrix, FIMatrix & Rules) ####
#############################################################################################

#' @include allClasses.R

## TAMatrix ##
##############

setMethod("length", "TAMatrix", function(x) {
  x@dim[1]
})

setMethod("print", "TAMatrix", function(x, descending = TRUE) {
  
  #collect all itemnames, print them by their frequency in a descending order
  print(data.frame(frequency = sort(rowSums(x@data), decreasing = descending)))
  
})

setMethod("show", "TAMatrix", function(object) {
  
  n <- length(object)
  cat("Found", n, "items. Use the print() to display\n")
  
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
  cat("Found", n, "frequent itemset(s). Use print() to display\n")
  
})

# combines the elements of both lists, i.e., a frequent item and it's corressponding support into a data frame
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

setMethod("summary", signature(object = "FIMatrix"), function(object) {
  
  n <- length(object)
  
  #Overview over frequent itemset matrix
  cat("\n")
  cat("Frequent itemsets in binary sparse matrix representation \n with", 
      nrow(object@data), "rows (items) and \n", 
      ncol(object@data), "columns (frequent itemsets)")
  cat("\n")
  
  #avoid unnecessary output when having less than 8 frequent itemsets
  if (n < 8) {
    
    #top n most frequent items
    cat("\n")
    cat("Most frequent items: \n" )
    print(sort(rowSums(object@data), decreasing = T)[1:n])
    cat("\n")
    
    #probability of observing top n items in an itemset
    cat("\n")
    cat("Observed frequency in frequent itemsets:\n")
    print(round(sort(rowSums(object@data), decreasing = T)[1:n] / n, 4))
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

#Plot itemsets against support
setMethod("plot", signature(x = "FIMatrix"), function(x, pch = 1, col = "red") {
  
  plot(colSums(x@data), x@support, 
       xlab = "Itemset length", ylab = "Support", 
       main = "Support distribution by itemset length", pch = pch, col = col)
  
})

setMethod("hist", "FIMatrix", function(x) {
  hist(colSums(x@data), main = "Histogram of frequent Itemsets", xlab = "Itemset length", col = "lightblue")
})

## Rules ## 
###########

setMethod("length", "Rules", function(x) {
  x@lhs@Dim[2]
})

setMethod("show", "Rules", function(object) {
  
  n <- length(object)
  if (n > 0) {
    cat("Found", n, "rule(s). Use print() to display\n")
  } else {
    cat("Found no rules. Try lowering the support and/or confidence threshold.\n")
  }

})

#display the rules in a way that a human can read them easily: 
#if lhs is purchased => rhs is frequently purchased, too (+support & confidence).
setMethod("print", "Rules", function(x,maxNumConsequent = 1,
                                     order_by = NULL, decreasing = TRUE) {
  
  if (length(x) == 0) {
    return("Found no rules. Try lowering the support and/or confidence threshold.")
  } else {
    ExtractRules(x, maxNumConsequent = maxNumConsequent,
                 order_by = order_by, decreasing = decreasing)
  }
  
})

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

#scatter plot of support against confidence, uses lift as color gradient
setMethod("plot", "Rules", function(x) {
  
  #color gradient function
  colfunc <- colorRampPalette(c("lightblue", "blue"))
  
  #ordering needed for color gradient
  plot.df <- data.frame(support = x@support, 
                        confidence = x@confidence,
                        lift = x@lift)
  
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

  
})

