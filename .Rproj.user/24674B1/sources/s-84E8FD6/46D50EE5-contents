#' Extract the frequent itemset from output of arules apriori package
#' 
#' This function does extract the sets and the support of a search for the frequent itemsets with arules apriori function
#' @name ExtractFrequentSets
#' @param Apriori_object This has to be the ouput of the apriori command from the arules package when looking for frequent itemsets
#' @return Gives back a list with sets, the frequent itemsets and support their corresponding support. The format is similar to the output of
#' ProjectApriori function FrequentItemsets.

ExtractFrequentSets <- function(Apriori_object){
  sets <- t(as(Apriori_object@items, "matrix"))
  support <- Apriori_object@quality[,1]
  return(list(sets = sets, support = support))
}
