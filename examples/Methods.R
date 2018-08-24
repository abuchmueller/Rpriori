##### This file contains a demonstration of how   #####
##### our package works. If ProjectApriori is     #####
##### properly installed this should run top to   #####
##### bottom without giving any errors however if #####
##### you wish, you can create the objects (run   #####
##### until line 27) and head straigt to the      #####
##### methods at the bottom (line 59 and below).  #####
#######################################################

library(ProjectApriori)

## First create objects of the appropriate classes ## 

data("Groceries")

#Sparse matrix of itemsets, where rows are items and columns transactions
#this is now fully optional, FindFrequentItemsets and AssociationRules now do this automatically
TransactionMatrix <- makeTransactionMatrix(Groceries)

#Sparse matrix of frequent itemsets + support vector containing support for each itemset
frequent_items <- FindFrequentItemsets(Groceries, 0.01)

#Object of 'Rule' class
Rules <- AssociationRules(Itemsets=Groceries, minsupport = 0.01, 
                          minconfidence = 0.4, arefrequent = FALSE)

#### from here on you can skip to the methods (line 59) ####

# using a frequent itemset matrix instead of a transaction matrix
# (faster since it doesn't have to recalculate frequent itemsets again)
fRules <- AssociationRules(FrequentItems = frequent_items, Itemsets = Groceries, minsupport = 0.03,
                          minconfidence = 0.4, arefrequent = TRUE)



#for comparison
aRules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.4));inspect(aRules)


#Check if classes are implemented correct
class(TransactionMatrix)
class(frequent_items)
class(Rules)

## check if we have methods listed for following classes: 
#  TAMatrix [our Transactionmatrix class], FIMatrix [our Frequent Itemset class], Rules [our Rules class]
showMethods("length")
showMethods("show")
showMethods("print")
showMethods("summary")
showMethods("plot")
showMethods("hist") # (only for FIMatrix)

## or check by class name directly
showMethods(class = "TAMatrix")
showMethods(class = "FIMatrix")
showMethods(class = "Rules")

#### Overview over all Methods #####

#TAMatrix
length(TransactionMatrix)
show(TransactionMatrix)
print(TransactionMatrix)
summary(TransactionMatrix)
plot(TransactionMatrix)

#FIMatrix
length(frequent_items)
show(frequent_items)
print(frequent_items)
summary(frequent_items)
plot(frequent_items)
hist(frequent_items)

#Rules
length(Rules)
show(Rules)
print(Rules)
summary(Rules)
plot(Rules) 
support(Rules)
confidence(Rules)
leverage(Rules)
lift(Rules)
extract(Rules)


