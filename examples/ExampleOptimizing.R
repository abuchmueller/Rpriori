library(profvis)
library()
debugonce(AssociationRules)

profvis({
result <- AssociationRules(Itemsets = Groceries, minsupport = 0.01, minconfidence = 0.2,  maxConsequentLength = 1)
})

print(result)




#Optimize GiveUniqueCol
## The use case right now takes about 9 seconds and is the majority of the runtime of Gen_candidates. #

# cand <- readRDS(system.file("testdata",'test_cand.rds', package="ProjectApriori"))
# 
# debugonce(GiveUniqueCol)
# start_time <- Sys.time()
# res <- GiveUniqueCol(cand)
# end_time <- Sys.time()
# 
# print(paste("Execution took",end_time - start_time, sep = ' '))

### 
# Det Support #
## currently 14 secs #
# data("Groceries")
# groc_trans <- ProjectApriori::makeTransactionMatrix(Groceries)
# 
# L2 <- readRDS(system.file("testdata","Perf_L2.rds", package="ProjectApriori"))
# 
# start_time <- Sys.time()
# res_1 <- DetSupport(L2, groc_trans)
# end_time <- Sys.time()
# 
# print(end_time - start_time)
# 
# 
# start_time <- Sys.time()
# res_2 <- DetSupport2(L2, groc_trans)
# end_time <- Sys.time()
# 
# print(paste("Execution took",end_time - start_time, sep = ' '))
# 
# all(res_1 == res_2)

####
# Optimize Gen Candidates
###

# For the use case in L1 we need right now 20 seconds #

# L1 <- readRDS(system.file("testdata","Perf_L1.rds", package="ProjectApriori"))
# 
# #debugonce(GenCandidates)
# start_time <- Sys.time()
# 
# res <- GenCandidates(L1)
# end_time <- Sys.time()
# 
# print(paste("Execution took",end_time - start_time, sep = ' '))




# # ## Example for optimizing. ##
# data("Groceries")
# groc_trans <- makeTansactionMatrix(Groceries)
# 
# rules_aprio <- apriori(Groceries,  parameter = list(support = 0.05, confidence = 0.1, minlen=2))
# 
# 
# #debugonce(AssociationRules)
# 
# start_time <- Sys.time()
# rules_praprio <- AssociationRules(Itemsets =  groc_trans,minsupport = 0.05, minconfidence = 0.1, arefrequent = FALSE)
# end_time <- Sys.time()
# print(paste("Execution took",end_time - start_time, sep = ' '))
# 
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

