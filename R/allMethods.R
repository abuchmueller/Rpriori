#### This file stores printing methods for all classes (Itemsets, FrequentItems & Rules) ####
#############################################################################################

#' @include allClasses.R

# This tells the generic print function what to print when the object is of class ngTMatrix
# Printing method for TransactionsMatrix class (later)
# For now it prints the names of the items in the TransactionMatrix


setMethod("print", signature(x = "TAMatrix"), function(x) {
  print(x@Dimnames[[1]])
})

setMethod("length", signature(x = "TAMatrix"), function(x) {
  return(x@dim[1])
})

# setMethod("summary", signature(x = "TAMatrix"), function(x) {
#   print(x@dim)
# })


## FrequentItems ##
# Idea here is to combine the elements of both lists, i.e., a frequent item and it's corressponding support and display them side by side column wise in a data frame

# Printing method for FrequentItems class
setMethod("print", signature(x = "FIMatrix"), function(x) {
  n <- x@data@Dim[1]
  output <- data.frame(items = rep(NA, n), support = rep(NA, n))
  for (i in 1:n) {
    output[i, 1] <- x@data@Dimnames[[1]][i]
    output[i, 2] <- x@support[i]
  }
  print(output)
})

## Rules ## 
# Idea here is to simply display the rules in a way that a human can read them easily: if lhs is purchased => rhs is frequently purchased, too (+support & confidence).

setMethod("print", signature(x = "Rules"), function(x) {
  ExtractRules(x, maxNumConsequent = 1)
})





