#' Determine all possible association rules with consequent length 1for given frequent transaction.
#' 
#' This function take the frequent transactions as input and generates rules of length len. It uses the Apriori-gen for more efficient generation
#' of rule candidates.
#' @name DetRules_1
#' @param Items Frequent Itemsets of different length, but at least two.
#' @param Items_support Here the support of the frequent itemsets should be stored.

DetRules_1 <- function(Items, Items_support){
  
  # Set a itemID so that I can recognise from which original set the rules come. #
  id <- 1:ncol(Items)
  
  # Here I save the number of items for each input set.
  item_n <- rep(NA, ncol(Items))
  cols <- 1:ncol(Items)
  for (i in cols){
    item_n[i] <- sum(Items@j == (i - 1))
  }
  
  # Here I save the positions at which the elements are not FALSE #
  pos_items <- apply(Items, 2, which)
  
  
  if (! is.list(pos_items)){
    pos_items <- lapply(seq_len(ncol(pos_items)), function(i) pos_items[,i])
  }
  
  # Length of the consequent of the rule is 1, therefore apriori cannot be used. #
    
    # Initialize ouput matrices 
    ncols <- sum(item_n)
    lhs <- out_mat <- sparseMatrix(i = c(),
                                   j = c(),
                                   giveCsparse = FALSE,
                                   dim = c(nrow(Items), ncols),
                                   dimnames = list(rownames(Items), NULL))
    rhs <- out_mat <- sparseMatrix(i = c(),
                                   j = c(),
                                   giveCsparse = FALSE,
                                   dim = c(nrow(Items), ncols),
                                   dimnames = list(rownames(Items), NULL))
    
    # for the left hand side reproduce the positions at which the matrix should be one. #
    cols <- 0:(ncols - 1) * nrow(Items)
    pos <- rep(cols, times = rep(item_n, times = item_n)) + unlist(rep(pos_items, times = item_n))
    lhs[pos] <- TRUE
    
    # Also only in the first iteration also give the initial support work since it did not change.
    Items_support <- rep(Items_support, times = item_n)
    id <- rep(id, times = item_n)
    
    # Now I will calcualte the positions at which I should set the value to False to create a subset of k - 1
    
    # lhs 
    pos <- 0:(ncols -1) * nrow(Items) + unlist(pos_items)
    lhs[pos] <- FALSE
    
    # rhs 
    # here the element I had to set to False have to be set to true.
    rhs[pos] <- TRUE
    
    
    return(list(lhs = lhs, rhs = rhs, support = Items_support, item_id = id, frequentItems = Items))
}

#' Determine all possible association rules with consequent length 1for given frequent transaction.
#' 
#' This function take the frequent transactions as input and generates rules of length len. It uses the Apriori-gen for more efficient generation
#' of rule candidates.
#' @name DetRules_K
#' @param rules List containing rhs, lhs, support, confidence, ID as well as the original frequent items of the rules.


DetRules_K <- function(rules){
  
  ##### Manual Header ######
  # rules <- R1
  ##########################
  
  # We can only create new rules for frequent itemsets that had at least two rules.
  # Two have two rules the item_id of the respective frequent itemset has to be repeated at least once.
  duplicated_cand <- duplicated(rules$item_id)|   duplicated(rules$item_id, fromLast = TRUE)
  
  if (!any(duplicated_cand)){
    
    # We cannot create any rules here. Give list of empty elements back.
    return(list(lhs = rules$lhs[,0], rhs = rules$rhs[,0], support = rules$support[0], item_id = rules$item_id[0], frequentItems = rules$frequentItems))
  } 

  # Only use the relevant elements #
  rules$lhs <- rules$lhs[,duplicated_cand]
  rules$rhs <- rules$rhs[,duplicated_cand]
  rules$support <- rules$support[duplicated_cand]
  rules$item_id <- rules$item_id[duplicated_cand]

  
  
  # For each frequent Itemset I have to create the possible m_item consquents based on its rules #
  list_cand <- list()

  # Save the total number of columns (items) #
  ncols <- 0
  nrows <- nrow(rules$lhs)
  ncols_each <- supp <- id<- rep(NA, length(unique(rules$item_id)))
  unique_ids <- unique(rules$item_id)
  

  # first I will create the unique matrix 
  for (f_it in 1:length(unique_ids)){

    list_cand[[f_it]] <- GenCandidates(rules$rhs[,rules$item_id == unique_ids[f_it], drop = FALSE])
    ncols <- ncols + ncol(list_cand[[f_it]])
    ncols_each[f_it] <- ncol(list_cand[[f_it]])
    supp[f_it] <- rules$support[rules$item_id == unique_ids[f_it]][1]
    id[f_it] <- rules$item_id[rules$item_id == unique_ids[f_it]][1]
  }
  
  # Create new support and item_id #
  supp <- rep(supp, times = ncols_each)
  id <- rep(id, times = ncols_each)
  
  # Create the new output matrices # 
  lhs <- out_mat <- sparseMatrix(i = c(),
                                 j = c(),
                                 giveCsparse = FALSE,
                                 dim = c(nrows, ncols),
                                 dimnames = list(rownames(rules$lhs), NULL))
  rhs <- out_mat <- sparseMatrix(i = c(),
                                 j = c(),
                                 giveCsparse = FALSE,
                                 dim = c(nrows, ncols),
                                 dimnames = list(rownames(rules$lhs), NULL))
  
  # If lhs and rhs are empty than we did not create any new elements and we can output the empty results #
  if (ncol(lhs) == 0 || ncol(rhs) == 0){
    return(list(lhs = lhs, rhs = rhs, support = supp, item_id = id, frequentItems = rules$frequentItems ))
  } else {
    
    # In the following we will need the relevant frequent itemsets that's why I will create a new variable for that.
    # the relevant frequent itemsets are the frequent itemset that do have rules for them as shown via item_id
    rel_frequentItems <- rules$frequentItems[,1:ncol(rules$frequentItems) %in% unique(rules$item_id), drop = FALSE]
    
    # If we have only one rel_frequentItems left we have a special case where we have to apply another logic #
    OnlyOnefrequ <- ncol(rel_frequentItems) == 1
    
    ## Left hand side value setting ##
    # Here we have to replicate the different frequent itemssets according to their appearance in ncols_each
    # The number of items each frequent itemset had.
    if (! OnlyOnefrequ){
      num_items <- apply(rules$frequentItems[,1:ncol(rules$frequentItems) %in% unique(rules$item_id)], 2, sum)
    } else {
      num_items <- sum(rules$frequentItems[,1:ncol(rules$frequentItems) %in% unique(rules$item_id)])
    }
    
    
    # For each value in true_elements its respective number of element because the columsn are going forward.
    if (! OnlyOnefrequ){
      col_pos <- rep(0:(ncols - 1) * nrows, times =  rep(num_items, times = ncols_each))
    } else {
      # only one column as candidate 
      col_pos <- rep(0:(ncols - 1) * nrows, num_items)
    }
    
    # Elements for each frequent itemset that were TRUE #
   
    
    # true elements from above is usually a list. But if the number of items in rel_frequent items for each transaction is the same
    # Than this becomes a matrix and we have to change the logic below.
    if(length(unique(num_items)) > 1) {
      true_elements <- apply(rel_frequentItems, 2, which)
    } else {
      true_elements <- lapply(split(rel_frequentItems,seq(ncol(rel_frequentItems))), which)
    }
    # Replicate this so that it does match the replication throught the GenCandidate process #
   true_elements <- unlist(rep(true_elements , times = ncols_each))
    
   
    
    # Add col_pos to it to account to for forward going columns and set the correct positions to one on the lhs.
    lhs[col_pos + true_elements] <- TRUE
    
    ## Right hand side value setting ##
    # Here we "map" the matrices in the list_cand to the true matrices 
    iter <- 1
    for (i in 1:length(ncols_each)){
      end <- iter + ncols_each[i]
      rhs[,iter:(end- 1)] <- list_cand[[i]]
      iter <- end
    }
    
    # Last but not least we have to "substract the rhs from the lhs in logical sense 
    # so that we do only have the frequent itemset as the union of the respective lhs and rhs
    lhs[rhs] <- FALSE
    
    # output #
    return(list(lhs = lhs, rhs = rhs, support = supp, item_id = id, frequentItems = rules$frequentItems ))
    
  }
}
