# library(arules)
# 
# data("Groceries")
# 
# # # compute frequent itemsets with apriori
# groc_apr <- apriori(Groceries, parameter = list(support = 0.08,  target="frequent itemsets"))
# res_arues <- inspect(groc_apr)
# 
# Groc <-  makeTansactionMatrix(Groceries)
# cands <- FrequentItemsets(Groc, minsupport = 0.08)
# res_me <- GetSets(cands)
# 
# # # print ordered result from apriori and me
# res_arues <- res_arues[order(res_arues$support, decreasing = TRUE),]
# res_me <- res_me[order(res_me$Support, decreasing = TRUE),]
# res_arues[,]
# res_me[,]





