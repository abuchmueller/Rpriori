#' Determine all possible association rules with consequent length 1 for given frequent transaction.
#' 
#' This function takes the frequent itemsets as input and generates rules of length 1.
#' It uses the Apriori-gen for more efficient generation of rule candidates.
#' @name DetRules_1
#' @param Items Frequent Itemsets of different length, but at least two.
#' @param Items_support Support of the frequent itemsets should be stored.
#' @return a list containing the rhs, lhs and support of the candidates. The vector Item_id shows
#' from which frequent itemset the specific rules comes from and is needed later on to determine 
#' the frequent itemsets.

DetRules_1 <- function(Items, Items_support){
  
  # This variable contains a unique number for each frequent input set. It is needed later on to
  # determine based on which frequent itemset a certain rules was created.
  id <- 1:ncol(Items)
  
  # Initiallize a variable that will contain the number of items for each itemset.
  item_n <- rep(NA, ncol(Items))
  
  # iterate over the columns (itemsets) in Items to determine the number of items in each of them.
  for (i in 1:ncol(Items)){
    item_n[i] <- sum(Items@j == (i - 1))
  }
  
  # Here I save the positions of the items in each itemset.
  pos_items <- apply(Items, 2, which)
  
  # The apply function does give a matrix if possible. But I need a list of vectors later on.
  # If this happened I convert the ouput back to list.
  # R is just the best ! <3 
  if (! is.list(pos_items)){
    pos_items <- lapply(seq_len(ncol(pos_items)), function(i) pos_items[,i])
  }
  
    # Initialize ouput matrices. 
    # item_n does contain the number of items in each itemset. All possible consequents of 
    # length one are therefore exacly the sum of that.
    # One rule is defined by column i of lhs and columns i of rhs where the items in rhs do 
    # describe the base items and the items in lhs the consequent.
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
    
    # The lhs side first rep the input columns by the number of items in each. To avoid the
    # rep function I manualle calculate the positions of the items that exist in the itemsets.
    cols <- 0:(ncols - 1) * nrow(Items)
    pos <- rep(cols, times = rep(item_n, times = item_n)) + unlist(rep(pos_items, times = item_n))
    
    # Set calculated positions of the matrix to true.
    lhs[pos] <- TRUE
    
    # The support of the input itemsets will be the same therfore I have to rep the input support
    # according to the number of items in each itemsets since that. The same logic applies to id
    # which does represent from which frequent itemset the candidate is generated.
    Items_support <- rep(Items_support, times = item_n)
    id <- rep(id, times = item_n)
    
    # Here I calculate the position of the itemsets on the rhs. This is simple since for each
    # itemset I have as many candidates as items in the original. Then I have have set for each
    # item only one element in the rhs to true.
    pos <- 0:(ncols -1) * nrow(Items) + unlist(pos_items)
    
    # If one "adds" the lhs to the rhs the result is the frequent itemset the rule is generated
    # from. Therefore, when I set one of the items true in the rhs I have to set it to false on 
    # the lhs.
    lhs[pos] <- FALSE
    rhs[pos] <- TRUE
    
    # Return all the generated items as a list.
    return(list(lhs = lhs, rhs = rhs, support = Items_support, item_id = id, frequentItems = Items))
}

#' Determine all possible association rules with consequent length 1for given frequent transaction.
#' 
#' This function take the frequent transactions as input and generates rules of length len. It uses the Apriori-gen for more efficient generation
#' of rule candidates.
#' @name DetRules_K
#' @param rules List containing rhs, lhs, support, confidence, ID as well as the original frequent items of the rules.


DetRules_K <- function(rules){

  # We can only created new rules for frequent itemsets that had at least two items
  # Two have two rules the item_id of the respective frequent itemset has to be repeated at least once.
  duplicated_cand <- duplicated(rules$item_id)|   duplicated(rules$item_id, fromLast = TRUE)
  
  if (!any(duplicated_cand)){
    
    # We cannot create any rules here. Give list of empty elements back.
    return(list(lhs = rules$lhs[,0], rhs = rules$rhs[,0], support = rules$support[0], item_id = rules$item_id[0], frequentItems = rules$frequentItems))
  } 

  # Only use the itemsets that have at least 2 items #
  rules$lhs <- rules$lhs[,duplicated_cand]
  rules$rhs <- rules$rhs[,duplicated_cand]
  rules$support <- rules$support[duplicated_cand]
  rules$item_id <- rules$item_id[duplicated_cand]

  # For each frequent Itemset I have to create the possible m_item consquents based on its rules
  # See that here we need to go through the frequent itemsets that may be represented by more than
  # one rule and respectivly columns on the left or right hand side.
  list_cand <- list()

  # initialize the variable 
  ncols <- 0
  
  # Save the total number of columns (items)
  nrows <- nrow(rules$lhs)
  
  # Initiallize the number of columns, support and id vector.
  ncols_each <- supp <- id<- rep(NA, length(unique(rules$item_id)))
  
  # This does contain the underlying frequent itemsets I will iterate trough to create
  # candidates of new rules for each.
  unique_ids <- unique(rules$item_id)
  

  # Iterate over the underlying frequent itemsets and for each generate rules with consequent 
  # length k + 1. Also save the number of colums (candidates), the support and id for each.
  for (f_it in 1:length(unique_ids)){

    list_cand[[f_it]] <- GenCandidates(rules$rhs[,rules$item_id == unique_ids[f_it], drop = FALSE])
    ncols <- ncols + ncol(list_cand[[f_it]])
    ncols_each[f_it] <- ncol(list_cand[[f_it]])
    supp[f_it] <- rules$support[rules$item_id == unique_ids[f_it]][1]
    id[f_it] <- rules$item_id[rules$item_id == unique_ids[f_it]][1]
  }
  
  # support and id should have the length of the lhs / rhs later on. That is for each frequent 
  # itemsets the number of candidates that was generated. This number is saved in ncols_each.
  supp <- rep(supp, times = ncols_each)
  id <- rep(id, times = ncols_each)
  
  # Create the new output matrices for the lhs, rhs.
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
  
  # If lhs and rhs are empty than we did not create any new candidates
  # and we can output the empty results #
  if (ncol(lhs) == 0 || ncol(rhs) == 0){
    return(list(lhs = lhs, rhs = rhs, support = supp, item_id = id,
                frequentItems = rules$frequentItems ))
  } else {
    
    # In the following we will need the relevant frequent itemsets that's why I will
    # create a new variable for that.
    # the relevant frequent itemsets are the frequent itemset that do have rules
    # for them as shown via item_id
    rel_frequentItems <- rules$frequentItems[,1:ncol(rules$frequentItems) 
                                             %in% unique(rules$item_id), drop = FALSE]
    
    # If we have only one rel_frequentItems left we have a special case
    # where we have to apply another logic #
    OnlyOnefrequ <- ncol(rel_frequentItems) == 1
    
    # Left hand side value setting
    # Here we have to replicate the different frequent itemssets according to their 
    # appearance in ncols_each that is the number of items in each frequent itemset.
    if (! OnlyOnefrequ){
      num_items <- apply(rules$frequentItems[,1:ncol(rules$frequentItems) %in%
                                               unique(rules$item_id)], 2, sum)
    } else {
      num_items <- sum(rules$frequentItems[,1:ncol(rules$frequentItems) %in%
                                             unique(rules$item_id)])
    }
    
    
    # Once again I will create a vector containing the positions in the matrix that have to
    # be set true in order to replicate the original itemsets. In col_pos I account for
    # the fact that the columns are going forward and therefore the position of the
    # items is higher.
    if (! OnlyOnefrequ){
      col_pos <- rep(0:(ncols - 1) * nrows, times =  rep(num_items, times = ncols_each))
    } else {
      
      # only one column as candidate 
      col_pos <- rep(0:(ncols - 1) * nrows, num_items)
    }
    
    # After I acounted for the columns I have to generate at which position for each column
    # the item is for the respecitve candidates.
    # true elements from above is usually a list. But if the number of items in rel_frequent items
    # for each transaction is the same Than this becomes a matrix and we have to
    # change the logic below.
    if(length(unique(num_items)) > 1) {
      true_elements <- apply(rel_frequentItems, 2, which)
    } else {
      true_elements <- lapply(split(rel_frequentItems,seq(ncol(rel_frequentItems))), which)
    }
    
    # Replicate this so that it does match the replication throught the GenCandidate process #
    true_elements <- unlist(rep(true_elements , times = ncols_each))
  
    # Add col_pos to it to account to for forward going columns and
    # set the correct positions to one on the lhs.
    # right now lhs does only reflect the replicated Frequent itemsets.
    lhs[col_pos + true_elements] <- TRUE
    
    
    # Here I go over the generated consequents by the apriori Gen and set the according columns
    # in the rhs to the generated consequents
    iter <- 1
    for (i in 1:length(ncols_each)){
      end <- iter + ncols_each[i]
      rhs[,iter:(end- 1)] <- list_cand[[i]]
      iter <- end
    }
    
    # Last but not least we have to "substract" the rhs from the lhs in logical sense 
    # so that we do only have the frequent itemset as the union of the respective lhs and rhs
    lhs[rhs] <- FALSE
    
    # output res resulting rules.
    return(list(lhs = lhs, rhs = rhs, support = supp, item_id = id, frequentItems = rules$frequentItems ))
    
  }
}
