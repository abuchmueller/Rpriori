# 
# ##################################
# # Comp with arules: simple ex. #
# ##################################
# 
# Try to set-up example with more than one two consquent rules
# input_sets <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
#          TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
#         FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
#          TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
#          TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),nrow = 6, dimnames = list( c("Bread", "Milk", "Diaper", "Beer", "Eggs", "Coke")))
# 
# result_arules <- apriori(t(input_sets), parameter = list(support = 0.3, confidence = 0.4, minlen=2))
# 
# result <- AssociationRules(Itemsets = input_sets, minsupport = 0.3, minconfidence = 0.4,  arefrequent = FALSE)
# me <- ExtractRules(result, maxNumConsequent = 1)
# arules <- inspect(result_arules)[order(result_arules@quality$support,result_arules@quality$confidence, decreasing = TRUE),]
# length(arules$support)
# length(me$Support)
# arules
# me


# ##################################
# # Comp with arules: Grocery data #
# ##################################
# # rm(list = ls())
# compute frequent itemsets with apriori
# data("Groceries")
# groc_trans <- makeTansactionMatrix(Groceries)
# groc_apr <- apriori(Groceries, parameter = list(support = 0.03,  target="frequent itemsets"))
# fr_tr <- ExtractFrequentSets(groc_apr)
# 
# rules_aprio <- apriori(Groceries,  parameter = list(support = 0.03, confidence = 0.2, minlen=2))
# rules_praprio <- AssociationRules(FrequentItems = fr_tr, Itemsets =  groc_trans,minsupport = 0.03, minconfidence = 0.2, arefrequent = TRUE)
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
# 
# # BUG REPORT: 
# # IT GIVE BACK RULES NOT WITH MINIMAL SUPPORT!

# ##################################
# # Comp with arules:  #
# ##################################

data("Epub")



groc_Epub <- makeTansactionMatrix(Epub)
apr_Epub <- apriori(Epub, parameter = list(support = 0.002,  target="frequent itemsets"))
fr_tr <- ExtractFrequentSets(apr_Epub)

rules_aprio <- apriori(Epub,  parameter = list(support = 0.002, confidence = 0.0001, minlen=2))
rules_praprio <- AssociationRules(FrequentItems = fr_tr, Itemsets =  groc_Epub,minsupport = 0.002, minconfidence = 0.0001, arefrequent = TRUE)

result_arules <- inspect(rules_aprio)
result_arules <- result_arules[order(result_arules$support, result_arules$confidence, decreasing = TRUE),]
result_praprio <- ExtractRules(rules_praprio, maxNumConsequent = 1)
length(result_praprio$Support)

result_arules
result_praprio

length(result_arules$support)
length(result_praprio$Support)
















