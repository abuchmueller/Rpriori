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
#Sparse matrix of itemsets, where rownames are itemnames
TransactionMatrix <- makeTransactionMatrix(Groceries)
#Sparse matrix of frequent itemsets + supportvector containing support for each itemset
frequent_items <- FrequentItemsets(TransactionMatrix, 0.01)
#Object of 'Rule' class (right now it's just 4 lists)
Rules <- AssociationRules(Itemsets=TransactionMatrix, minsupport = 0.03, 
                          minconfidence = 0.4, arefrequent = FALSE)
Rules <- AssociationRules(FrequentItems = frequent_items, Itemsets = TransactionMatrix, minsupport = 0.03,
                          minconfidence = 0.4, arefrequent = TRUE)

##This should use generic print and output the rules nicely (Billo Version)
print.rules <- function(x) {
  ExtractRules(x)
}

class(TransactionMatrix)
class(frequent_items)
class(Rules)

## check if we have methods listed for following classes: TransactionMatrix [currently ngTMatrix], FrequentItems [currently list], Rules [currently list]
showMethods("print")

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
rules.arules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5));inspect(rules.arules)


## Methods ##

#TAMatrix
length(TSM)
show(TSM)
print(TSM)
summary(TSM)
plot(TSM)

#FIMatrix
length(fitems)
show(fitems)
print(fitems)
summary(fitems)
plot(fitems)
hist(fitems)

#Rules
length(rulez)
show(rulez)
print(rulez)
summary(rulez)
plot(rulez)

