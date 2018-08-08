#' Get frequent itemsets with support from output of frequentItemset object from arules.
#' 
#' This function takes the ouput of the frequentItemsets function from arules and creates
#' a list of the frequent itemsets with their support from it.
#' @name GetSets
#' @export
#' @param items Output of the frequentItemsets function from arules
#' @return A dataframe of all the frequent Itemsets with their respective support.

GetSets <- function(items){
  
  names <- apply(items[[1]], 2, function(set){
    return(paste(names(set)[set], collapse = ','))
  })
  return(data.frame("Items" = names, "Support" = items[[2]]))
}
