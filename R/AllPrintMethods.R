#### This file stores printing methods for all classes (Itemsets, FrequentItems & Rules) ####
#############################################################################################

# This tells the generic print function what to print when the object is of class ngTMatrix
# Printing method for TransactionsMatrix class (later)
# For now it prints the names of the items in the TransactionMatrix

setMethod("print", signature(x = "ngTMatrix"), function(x) {
  print(x@Dimnames[[1]])
})


# Printing method for transactions class
setMethod("print", signature(x = "transactions"), function(x) {
  print(x@itemInfo)
})

## FrequentItems ##
# Idea here is to combine the elements of both lists, i.e., a frequent item and it's corressponding support and display them side by side column wise in a data frame

# setClass() ## Placeholder to implement FrequentItems properly as an S4 class later on

# Printing method for FrequentItems class
setMethod("print", signature(x = "FrequentItems"), function(x) {
  n <- x$sets@Dim[1]
  output <- data.frame(items = rep(NA, n), support = rep(NA, n))
  for (i in 1:n) {
    output[i, 1] <- x$sets@Dimnames[[1]][i]
    output[i, 2] <- x$support[i]
  }
  print(output)
})

## Rules ## 
# Idea here is to simply display the rules in a way that a human can read them easily: if lhs is purchased => rhs is frequently purchased, too (+support & confidence).

# setClass()  #Placeholder to implement Rules properly as an S4 class later on

setMethod("print", signature(x = "Rules"), function(x) {
  print(ExtractRules(Rules, maxNumConsequent = 1))
})




