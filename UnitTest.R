rm(list = ls())
library(arules)
 

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

