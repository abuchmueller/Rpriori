##### This file contains a demonstration of how   #####
##### our package works. If Rpriori is properly   #####
##### installed this should run top to bottom     #####
##### without any errors.                         #####
#######################################################

library(Rpriori)
library(arules)

data("Groceries")

# This creates a sparse matrix of itemsets, where rows are items and columns transactions
# this is fully optional, FindFrequentItemsets() and AssociationRules() now do this automatically
TAM <- makeTAMatrix(Groceries)

# Creates sparse matrix of frequent itemsets and a support vector containing support for each itemset
# this is also optional; it is the same as the matrix in the @FrequentItemsets slot in a Rules object
Frequent <- FindFrequentItemsets(Groceries, 0.01)

# Create an object of 'Rule' class
Rules <- AssociationRules(Groceries, minsupport = 0.01)

# using a frequent itemset matrix instead of a transaction matrix
# (might faster since it doesn't have to recalculate frequent itemsets 
# again if you dont lower the support threshold)
fRules <- AssociationRules(Groceries, Frequent, minsupport = 0.03,
                          minconfidence = 0.4)



#for comparison
aRules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.4));inspect(aRules)


# Check if classes are implemented correct
class(TAM)
class(Frequent)
class(Rules)

## check if we have methods listed for following classes: 
#  TAMatrix (our Transactionmatrix class), FIMatrix (our frequent itemmatrix class), Rules (our Rules class)
showMethods("length")
showMethods("show")
showMethods("print")
showMethods("summary")
showMethods("plot")
showMethods("hist")
showMethods("qplot")

## or check by class name directly
showMethods(classes = "TAMatrix")
showMethods(classes = "FIMatrix")
showMethods(classes = "Rules")

#### Overview over all methods #####

#TAMatrix
length(TAM)
show(TAM)
print(TAM)
summary(TAM)
plot(TAM)
qplot(TAM)

#FIMatrix
length(Frequent)
show(Frequent)
print(Frequent)
summary(Frequent)
plot(Frequent)
hist(Frequent)
qplot(Frequent, type = "scatter", col = "blue", alpha = 0.25) #You don't need to supply more than a valid object here but you can
qplot(Frequent, type = "hist")

#Rules
length(Rules)
show(Rules)
print(Rules)
summary(Rules)
plot(Rules) 
qplot(Rules)
support(Rules)
confidence(Rules)
leverage(Rules)
lift(Rules)
extract(Rules) # These are the underlying frequent itemsets used in rule creation

