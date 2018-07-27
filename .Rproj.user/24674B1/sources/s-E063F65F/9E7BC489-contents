# library(arules)
# 
# rm(list = ls())
# 
# data("Groceries")
# 
# 
# # compute frequent itemsets with apriori
# groc_apr <- apriori(Groceries, parameter = list(support = 0.02,  target="frequent itemsets"))
# res_arues <- inspect(groc_apr)
# 
# # compute frequent itemsets with my function
# Groc <-  makeTansactionMatrix(Groceries)
# cands <- FrequentItemsets(Groc, minsupport = 0.02)
# res_me <- Getsets(cands)
# res_me <- res_me[res_me$Support != 1.00000000,]
# 
# # print ordered result from apriori and me
# res_arues <- res_arues[order(res_arues$support),]
# res_me <- res_me[order(res_me$Support),]
# res_arues[23,]
# res_me[,]
# 
# apply(cands$sets, 2, sum)
# 
# apply(groc_apr@items, 2, sum)
# 
# cands$sets
