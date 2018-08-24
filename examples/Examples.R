# 
# ##################################
# # Comp with arules: simple ex. #
# ##################################
# 
# Try to set-up example with more than one two consquent rules
input_sets <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
         TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
        FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
         TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
         TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6, dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))

result_arules <- apriori(t(input_sets), parameter = list(support = 0.3, confidence = 0.4, minlen=2))

input_sets_spares <- makeTransactionMatrix(input_sets)

result <- AssociationRules(Itemsets = input_sets_spares , minsupport = 0.3, minconfidence = 0.4,  arefrequent = FALSE)

me <- ExtractRules(result, maxNumConsequent = 1)
arules <- inspect(result_arules)[order(result_arules@quality$support,result_arules@quality$confidence, decreasing = TRUE),]
length(arules$support)
length(me$Support)
arules
me


# ##################################
# # Comp with arules: Grocery data #
# ##################################
# rm(list = ls())
# # compute frequent itemsets with apriori
data("Groceries")



start_time <- Sys.time()
rules_aprio <- apriori(Groceries,  parameter = list(support = 0.01, confidence = 0.2, minlen=2))
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
rules_praprio <- AssociationRules(Itemsets =  Groceries, minsupport = 0.01, minconfidence = 0.2,
                                  arefrequent = FALSE, maxConsequentLength = 1)
end_time <- Sys.time()
print(end_time - start_time)

print(rules_praprio)


colSums(rules_praprio@rhs)

# 
# profvis({
#   rules_aprio <- apriori(Groceries,  parameter = list(support = 0.03, confidence = 0.2, minlen=2))
# })
# # 
profvis({
  rules_praprio <- AssociationRules(Itemsets =  Groceries ,minsupport = 0.005, minconfidence = 0.2, arefrequent = FALSE)
})


# 
# 
# 
# 
result_arules <- inspect(rules_aprio)
result_arules <- result_arules[order(result_arules$support, result_arules$confidence, decreasing = TRUE),]
result_praprio <- ExtractRules(rules_praprio, maxNumConsequent = 1)
length(result_praprio$Support)
length(result_arules$support)
# 
result_arules
result_praprio
# 
length(result_arules$support)
length(result_praprio$Support)


# ##################################
# # Comp with arules:  #
# ##################################
# 
data("Epub")

apr_Epub <- apriori(Epub, parameter = list(support = 0.01,  target="frequent itemsets"))
fr_tr <- ExtractFrequentSets(apr_Epub)

length(fr_tr$support)
rownames(fr_tr$sets[rowSums(fr_tr$sets) > 0,]) == rownames(FrequentItemsets(groc_Epub, 0.01)@data)



# 
# 
# rules_aprio <- apriori(Epub,  parameter = list(support = 0.002, confidence = 0.0001, minlen=2))
# rules_praprio <- AssociationRules(FrequentItems = fr_tr, Itemsets =  groc_Epub,minsupport = 0.002, minconfidence = 0.0001, arefrequent = TRUE)
# 
# result_arules <- inspect(rules_aprio)
# result_arules <- result_arules[order(result_arules$support, result_arules$confidence, decreasing = TRUE),]
# result_praprio <- ExtractRules(rules_praprio, maxNumConsequent = 1)
# length(result_praprio$Support)
# 
# result_arules
# result_praprio
# 
# length(result_arules$support)
# length(result_praprio$Support)








## Issue: ###




