#' Extract the frequent itemset from output of arules apriori package
#' 
#' This function does extract the sets and the support of a search for the frequent itemsets
#'  with arules apriori function
#' @name ExtractFrequentSets
#' @param Apriori_object This has to be the ouput of the apriori command from the arules
#'  package when looking for frequent itemsets
#' @return Gives back a list with sets, the frequent itemsets and  their corresponding
#'  support. The format is similar to the output of ProjectApriori function FrequentItemsets.

ExtractFrequentSets <- function(Apriori_object){
  
  # Extract the sparse matrix object from the apriori object.
  # This is a compressed sparse matrix and since the package Project_Apriori does work with 
  # not compressed sparse matrices I do convert It.
  sets <- as(Apriori_object@items@data, "TsparseMatrix")
  
  # Also extrac the labels of the items and set them as the rownames of the 
  # created matrix.
  rownames(sets) <- Apriori_object@items@itemInfo$labels
  
  # Extract the support of the Apriori Object.
  support <- Apriori_object@quality[,1]
  
  # return the extracted itemsets and the support as a list. This is also the output of
  # the FrequentItemsets function.
  return(list(sets = sets, support = support))
}
