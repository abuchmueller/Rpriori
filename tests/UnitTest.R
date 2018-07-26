rm(list = ls())

# Load relevant packages #
library(arules)
library(testthat)


##### TEST: AprioriGen #####
# In this test I will use the example from wikipedia #
# Testing set-up based on example from wikipedia #
# This does represent the example candidate set from wikipedia "https://de.wikipedia.org/wiki/Apriori-Algorithmus" in a incident matrix #
testmat <- matrix(as.logical(c(1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0)), nrow = 5, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL))

# The result should be #
result_mat <- matrix(c(TRUE, TRUE, TRUE, TRUE, FALSE), ncol = 1, dimnames = list(c('a', 'b', 'c', 'd', 'e'),NULL))



##### TEST: FREQUENT ITEMSETS #####
## Get Sample data ##
data("Groceries")

## Run aprioi on it from arules ##
Groceries_arules <- apriori(Groceries, parameter = list(confidence = 0.5, support = 0.01))

## Generate the frequent itemsets from it. ##
itemsets <- unique(generatingItemsets(Groceries_arules))
itemsets.df <- as(itemsets, "data.frame")
frequentItemsets <- itemsets.df[with(itemsets.df, order(-support,items)),]
frequentItemsets

# These itemset should also be the result of your apriori. #

## run my Frequent Itemset on it ##

