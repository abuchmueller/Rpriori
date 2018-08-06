###
# Optimize GiveUniqueCol
### The use case right now takes about 9 seconds and is the majority of the runtime of Gen_candidates. #
# 
# cand <- readRDS(system.file("testdata",'test_cand.rds', package="ProjectApriori"))
# 
# debugonce(GiveUniqueCol)
# start_time <- Sys.time()
# 
# res <- GiveUniqueCol(cand)
# end_time <- Sys.time()
# 
# print(paste("Execution took",end_time - start_time, sep = ' '))

### 
# Det Support #
## currently 14 secs #
# data("Groceries")
# groc_trans <- makeTansactionMatrix(Groceries)
# 
# L2 <- readRDS(system.file("testdata","Perf_L2.rds", package="ProjectApriori"))
# 
# start_time <- Sys.time()
# res_1 <- DetSupport(L2, groc_trans)
# end_time <- Sys.time()
# 
# print(paste("Execution took",end_time - start_time, sep = ' '))
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

L1 <- readRDS(system.file("testdata","Perf_L1.rds", package="ProjectApriori"))

#debugonce(GenCandidates)
start_time <- Sys.time()

res <- GenCandidates(L1)
end_time <- Sys.time()

print(paste("Execution took",end_time - start_time, sep = ' '))
# 
# 



# # ## Example for optimizing. ##
# data("Groceries")
# groc_trans <- makeTansactionMatrix(Groceries)
# 
# rules_aprio <- apriori(Groceries,  parameter = list(support = 0.07, confidence = 0.1, minlen=2))
# 
# 
# 
# start_time <- Sys.time()
# debugonce(AssociationRules)
# rules_praprio <- AssociationRules(Itemsets =  groc_trans,minsupport = 0.07, minconfidence = 0.2, arefrequent = FALSE)
# end_time <- Sys.time()
# print(paste("Execution took",end_time - start_time, sep = ' '))
# 
# rules_praprio <- AssociationRules(FrequentItems = fr_tr, Itemsets =  groc_trans,minsupport = 0.03, minconfidence = 0.2, arefrequent = TRUE)