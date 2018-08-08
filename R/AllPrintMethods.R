#### This file stores printing methods for all classes (Itemsets, FrequentItems & Rules) ####
#############################################################################################


## check if we have methods listed for following classes: TransactionMatrix [currently ngTMatrix], FrequentItems [currently list], Rules [currently list]
showMethods("print") #Edit out later

# This tells the generic print function what to print when the object is of class ngTMatrix
# Printing method for TransactionsMatrix class 
# For now it prints the names of the items on the TransactionMatrix

setMethod("print", signature(x = "ngTMatrix"), function(x) {
  print(x@Dimnames[[1]])
})
print(TransactionMatrix)

# Printing method for transactions class
setMethod("print", signature(x = "transactions"), function(x) {
  print(x@itemInfo)
})

## FrequentItems
# Idea here is to combine the elements of both lists, i.e., a frequent item and it's corressponding support and display them side by side column wise in a data frame

# setClass() ## Placeholder to implement FrequentItems properly as an S4 class later on

# For now we create an S3 class from frequent_items and coerce it into an S4 class
class(frequent_items) <- "FrequentItems"
setOldClass("FrequentItems")
isS4(frequent_items)

# Printing method for FrequentItems class
setMethod("print", signature(x = "FrequentItems"), function(x) {
  output <- data.frame(items = rep(NA, x$sets@Dim[1]), support = rep(NA, x$sets@Dim[1]))
  for (i in 1:x$sets@Dim[1]) {
    output[i, 1] <- x$sets@Dimnames[[1]][i]
    output[i, 2] <- x$support[i]
  }
  print(output)
})
print(frequent_items)


## Rules
# Idea here is to simply display the rules in a way that a human can read them easily: if lhs is purchased => rhs is frequently purchased, too (+support & confidence).

# setClass()  #Placeholder to implement Rules properly as an S4 class later on

# For now we create an S3 class from Rules and coerce it into an S4 class 
class(Rules) <- "Rules"
setOldClass("Rules") # currently not working as Rules stays an S4 class object the method however works fine afterwards
isS4(Rules)

setMethod("print", signature(x = "Rules"), function(x) {
  print(ExtractRules(Rules))
})
print(Rules)



