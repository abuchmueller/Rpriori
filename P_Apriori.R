rm(list = ls())
library(arules)
 

## Get Sample data ##
data("Groceries")


# The following function will take a arules transaction matrix and create a normal matrix from it #
# This does not make any sense performance wise but in order to create the first version of our code it is easier to understand #
makeTansactionMatrix <- function(mat){
  
  # Get the underlying data matrix from the transaction matrix 
  mat_out <- t(as.matrix(mat@data))
  
  # Add the rownames (which do represent the different items)
  colnames(mat_out) <- mat@itemInfo$labels


  # return the  resulting matrix #
  return(mat_out)
}



Groc <-  makeTansactionMatrix(Groceries)

f_items <- function(dataset, minsupport){
  
  # Calculate frequent Itemsets of size 1 #
  L1 <- apply(dataset,2 , mean)
  L1 <- L1[L1 >= minsupport]
  
  # 
  k <- 2
  while (length(get(paste("L", k - 1))) > 0 ){
    
    assign(paste("L", k), ...)
    
    k <- k + 1
  }
  
  return(L1)
  
}

### Function call ###
names(f_items(Groc, 0.1))


## Apriori-Gen function ##




  