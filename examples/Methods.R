##### This file contains examples for all methods #####
#######################################################

library(devtools)
library(roxygen2)
library(testthat)
library(Rcpp)
library(ProjectApriori)
library(profvis)
library(pryr)

data("Groceries")
TransactionMatrix <- makeTransactionMatrix(Groceries) #Sparse matrix of itemsets, where rownames == itemnames
frequent_items <- FrequentItemsets(TransactionMatrix, 0.01)# Sparse matrix of frequent itemsets + supportvector containing support for each itemset
Rules <- AssociationRules(Itemsets=TransactionMatrix, minsupport = 0.01, minconfidence = 0.5, arefrequent = FALSE) #Object of 'Rule' class (right now it's just 4 lists)
print.rules <- function(x) {
  ExtractRules(x) ##This should use generic print and output the rules nicely (Billo Version)
}

class(TransactionMatrix)
class(frequent_items)
class(Rules)

## check if we have methods listed for following classes: TransactionMatrix [currently ngTMatrix], FrequentItems [currently list], Rules [currently list]
showMethods("print") 

# For now we create an S3 class from frequent_items and coerce it into an S4 class (Reactivate this block after building package)
class(frequent_items) <- "FrequentItems" 
setOldClass("FrequentItems") 
isS4(frequent_items)

# For now we create an S3 class from Rules and coerce it into an S4 class (Reactivate this block after building package)
class(Rules) <- "Rules"
setOldClass("Rules") # currently not working as Rules stays an S4 class object the method however works fine afterwards
isS4(Rules) 


#Preview how the output looks currently
print(TransactionMatrix)
print(frequent_items)
print(Rules)


## Classes ##
# this creates a TAMatrix object from a sparse Matrix from the makeTransactionMatrix function
TSM <- new("TAMatrix", data  = TransactionMatrix, 
            dim = TransactionMatrix@Dim, items = TransactionMatrix@Dimnames[[1]])

#Frequent Itemsets
fitems <- new("FIMatrix", data = frequent_items$sets, support = frequent_items$support)

#Rules
rulez <- new("Rules", lhs = Rules$lhs, rhs = Rules$rhs, support = Rules$support, confidence = Rules$confidence)


