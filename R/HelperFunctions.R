# ------------------------------------------------------------------------------------------------ #
# -------------------------------- Helper functions for Rpriori----------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------ #
# ------------------------------------------ Duplicate ------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# This function is based on the C routine R_pnindex from the arules package. Within that package
# the function is used to determine duplicate itemsets, that are columns, in a ngCMatrix. Our 
# algorithm is dependend on a "unique" function to find the unique column of a sparse matrix. 
# Unfortunately, the Matrix package does not provide such a function and the R base function 
# "unique" with MARGIN = 1 does not work for sparse matrices. Also, we were unfortunately not able
# to program a function in R that was fast enough. Therefore, we decided to inlcude that function
# from the arules package.

#' Find duplicate columns in a sparse matrix
#' 
#' This function is based on a c function from the arules package that finds duplicated columns
#' in a sparse matrix
#' @useDynLib Rpriori R_pnindex
#' @name Duplicate
#' @param mat A sparse matrix of type ngTMatrix, lgCMatrix or ngCMatrix.
#' @return A boolean vector that does have one element for each column in the matrix. True does
#' represent a column that is replicated, False means it is not replicated.

Duplicate <- function(mat){
  
  # If the matrix is not a compressed aparse matrix, make it one! :)
  if (class(mat)[1] == "ngTMatrix") {
    mat <- sparseMatrix(i = mat@i,
                        j = mat@j,
                        giveCsparse = TRUE,
                        dims = c(nrow(mat), ncol(mat)),
                        index1 = FALSE,
                        dimnames = list(rownames(mat), NULL))
  }
  
  if (class(mat)[1] == "lgCMatrix") {
    mat <- sparseMatrix(i = mat@i,
                        p = mat@p,
                        giveCsparse = TRUE,
                        dims = c(nrow(mat), ncol(mat)),
                        index1 = FALSE,
                        dimnames = list(rownames(mat), NULL))
  }
  
  # Check whether correct class supplied since the C routine does only work with matrices of the 
  # class ngCmatrix (sparse, column oriented compressed matrices).
  if (class(mat)[1] == "ngCMatrix") {
    
    # call the c routine that give a unique ID for each unqiue column. Therefore, If 
    # a column is repeated the ID is repeated. 
    mat_col <- .Call(R_pnindex, mat, NULL, FALSE)
    
    # applying duplicated gives whether some of the IDs are repeated and therefore the
    # repeated columns. By doing this we automatically select the first columns of the 
    # repeated column (we could change this in duplicated)
    return(duplicated(mat_col))
  } else {
    
    # In this branch the supplied matrix was not of type ngTMatrix or lgCMatrix and converted 
    # or of type ngCMatrix, therefore is not supported. Give Error!
    stop(paste("The class ", class(mat)[1], "is unknown to the Duplicate function"))
  }
}

# ------------------------------------------------------------------------------------------------ #
# ---------------------------------------- DetRules_1 -------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Determine all possible association rules with consequent length 1 for given frequent itemsets
#' 
#' This function takes frequent itemsets as input and generates all rules of consequent length 1 
#' that have to be considered for rule mining. It uses the Apriori-gen for more efficient
#' generation of rule candidates.
#' @name DetRules_1
#' @param Items Object of class FIMatrix containing frequent itemsets of different length. Minimal
#' length should be two.
#' @return Object of class Rules that contains all relevant rules with consequent length 1.
DetRules_1 <- function(Items) {
  
  # This variable contains a unique number for each frequent input set. It is needed later on to
  # determine based on which frequent itemset a certain rules was created.
  id <- 1:ncol(Items)
  
  # Determine the number in each itemset.
  item_n <- colSums(Items)
  
  # Here I save the positions of the items in each itemset.
  pos_items <- apply(Items@data, 2, which)
  
  # The apply function does give a matrix if possible (all output vectors of same length). But I
  # need a list of vectors later on If this happens I convert the ouput back to list.
  if (!is.list(pos_items)) {
    pos_items <- lapply(seq_len(ncol(pos_items)), function(i) pos_items[,i])
  }
  
  # Initialize output matrices. 
  ncols <- sum(item_n)
  lhs <- sparseMatrix(i = c(),
                      j = c(),
                      giveCsparse = FALSE,
                      dims = c(nrow(Items), ncols),
                      dimnames = list(items(Items), NULL))
  
  rhs <- sparseMatrix(i = c(),
                      j = c(),
                      giveCsparse = FALSE,
                      dims = c(nrow(Items), ncols),
                      dimnames = list(items(Items), NULL))
  
  # The lhs side first repeat the input columns by the number of items in each. To avoid the
  # rep function I manually calculate the positions of the items that exist in the itemsets.
  cols <- 0:(ncols - 1) * nrow(Items)
  pos <- rep(cols, times = rep(item_n, times = item_n)) + unlist(rep(pos_items, times = item_n))
  
  # Set calculated positions of the matrix to true.
  lhs[pos] <- TRUE
  
  # The support of the input itemsets will be the same therfore I have to repeat the input support
  # according to the number of items in each itemsets since that. The same logic applies to id
  # which does represent from which frequent itemset the candidate rule is generated.
  Items_support <- rep(Items@support, times = item_n)
  id <- rep(id, times = item_n)
  
  # Here I calculate the position of the itemsets on the rhs. This is simple since for each
  # itemset I have as many candidates as items in the original. Then I have have set for each
  # item only one element in the rhs to true.
  pos <- 0:(ncols - 1) * nrow(Items) + unlist(pos_items)
  
  # If one "adds" the lhs to the rhs the result is the frequent itemset the rule is generated
  # from. Therefore, when I set one of the items true in the rhs I have to set it to false on 
  # the lhs.
  lhs[pos] <- FALSE
  rhs[pos] <- TRUE
  
  # Return all the generated items as object of class Rules. Set all values of confidence,
  # lift and leverage to -1 since they will be calculated later on.
  return(new('Rules',
             lhs = lhs,
             rhs = rhs,
             support = Items_support,
             confidence = rep(0, length(Items_support)),
             lift = rep(0, length(Items_support)),
             leverage = rep(0, length(Items_support)),
             itemsetID = id,
             FrequentItemsets = new("FIMatrix",
                                    data = Items@data,
                                    support = Items@support)))
}

# ------------------------------------------------------------------------------------------------ #
# ---------------------------------------- DetRules_k -------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Determine all possible association rules with consequent length > 1 for given frequent itemset.
#' 
#' This function takes the rules with consequent length k as input and generates rules of 
#' consequent length K + 1. For a rule having consequent length k it will return, if possible,
#' rules with consequent length  k + 1. It uses the Apriori-gen for more efficient generation 
#' of rule candidates.
#' @name DetRules_K
#' @param rules Object of class Rules containing the current rules with consequent length k
DetRules_K <- function(rules) {
  
  # We can only created new rules for frequent itemsets that had at least two items
  # To have two rules the itemsetID of the respective frequent itemset has to be repeated at least
  # once.
  duplicated_cand <- duplicated(rules@itemsetID) | duplicated(rules@itemsetID, fromLast = TRUE)
  
  if (!any(duplicated_cand)) {
    
    # We cannot create any rules here. Give empyt object of class Rules back.
    return(select(rules,0,0))
  } 
  
  # Only use the itemsets that have at least 2 items #
  rules <- select(rules,NULL,duplicated_cand)
  
  # For each frequent Itemset I have to create the possible m_item consquents based on its rules
  # See that here we need to go through the frequent itemsets that are represented by more than
  # one rule / column on the left or right hand side.
  list_cand <- list()
  
  # initialize the variable 
  ncols <- 0
  
  # Save the total number of columns (items)
  nrows <- nrow(rules)
  
  # Initiallize the number of columns, support and id vector.
  ncols_each <- supp <- id <- rep(NA, length(unique(rules@itemsetID)))
  
  # This does contain the underlying frequent itemsets I will iterate trough to create
  # candidates of new rules for each.
  unique_ids <- unique(rules@itemsetID)
  
  # Iterate over the underlying frequent itemsets and for each generate rules with consequent 
  # length k + 1. Also save the number of colums (candidates), the support and id for each.
  for (f_it in 1:length(unique_ids)) {
    
    list_cand[[f_it]] <- GenCandidates(rules@rhs[,rules@itemsetID == unique_ids[f_it], drop = FALSE])
    ncols <- ncols + ncol(list_cand[[f_it]])
    ncols_each[f_it] <- ncol(list_cand[[f_it]])
    supp[f_it] <- rules@support[rules@itemsetID == unique_ids[f_it]][1]
    id[f_it] <- rules@itemsetID[rules@itemsetID == unique_ids[f_it]][1]
  }
  
  # support and id should have the length of the lhs / rhs later on. That is for each frequent 
  # itemsets the number of candidates that was generated. This number is saved in ncols_each.
  supp <- rep(supp, times = ncols_each)
  id <- rep(id, times = ncols_each)
  
  # Create the new output matrices for the lhs, rhs.
  # for the lhs we first select from columns from the frequent itemsets. We use the vector id to do
  # so since in it it it stored from which frequent itemset the rules was created.
  lhs <- select(rules@FrequentItemsets,NULL,id)@data
  rhs <- sparseMatrix(i = c(),
                      j = c(),
                      giveCsparse = FALSE,
                      dims = c(nrows, ncols),
                      dimnames = list(items(rules), NULL))
  
  # If lhs and rhs are empty than we did not create any new candidates
  # and we can output the empty results #
  if (ncol(lhs) == 0 || ncol(rhs) == 0) {
    return(new("Rules",
               lhs = lhs,
               rhs = rhs,
               support = supp,
               confidence = rep(0, length(supp)),
               lift = rep(0, length(supp)),
               leverage = rep(0, length(supp)),
               itemsetID = id,
               FrequentItemsets = rules@FrequentItemsets))
  } else {
    
    # Here I go over the generated consequents by the apriori Gen and set the according columns
    # in the rhs to the generated consequents
    iter <- 1
    for (i in 1:length(ncols_each)) {
      end <- iter + ncols_each[i]
      rhs[,iter:(end - 1)] <- list_cand[[i]]
      iter <- end
    }
    
    # Last but not least we have to "substract" the rhs from the lhs in logical sense 
    # so that we do only have the frequent itemset as the union of the respective lhs and rhs
    lhs[rhs] <- FALSE
    
    # output res resulting rules.
    return(new("Rules",
               lhs = lhs,
               rhs = rhs,
               support = supp,
               confidence = rep(0, length(supp)),
               lift = rep(0, length(supp)),
               leverage = rep(0, length(supp)),
               itemsetID = id,
               FrequentItemsets = rules@FrequentItemsets))
  }
}

# ------------------------------------------------------------------------------------------------ #
# --------------------------------------- CombineFIMatrix----------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Combining list of FIMatrices containing frequent itemsets with different length.
#' 
#' This functions takes a list of FIMatrices matrices as input. The FIMatrices may have
#'  different number or rows (different items), different number of columns 
#'  (different number of itemsets) as well es different number of items per itemset. Then these
#'   matrices are combined to a big, sparse incident matrix that does have all rows and columns
#'    of the input matrices and a FIMatrix is returned.
#' @name CombineFIMatrix
#' @param list_input List of FIMatrix. These matrices may have different number 
#' of rows, columns and number of items
#' @return Another object of class FIMatrix that contains all the input Itemsets.
CombineFIMatrix <- function(list_input) {
  # Determine the dimensions of the result matrix
  # It should have all rows, columns from all input matrices
  
  # Collect the number of itemsets / columns in ncols.
  ncols <- 0
  
  # collect the names of the different items in items
  items <- c()
  
  # Iterate over the list and add the number of columns as well as the items.
  for (cand in list_input) {
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, items(cand)))
  }
  
  # Create the output matrix based on the dimension calculated above.
  # The matrix is created completely empty and will be filled later.
  res_mat <- new('FIMatrix',
                 data = sparseMatrix(i = c(),
                                     j = c(),
                                     giveCsparse = FALSE,
                                     dims = c(length(items), ncols),
                                     dimnames = list(items, NULL)),
                 support = rep(0, ncols))
  
  # Iterate again over the matrices in the input list and fill the output matrix with 
  # the correct entries from the matrices.
  
  # Initialize column counter that keeps track of which columns are currently overwritten
  # in the output matrix.
  cur_col <- 1
  
  for (cand in list_input) {
    if (ncol(cand@data) > 0) {
      
      # It is ensured that the current matrix is not empty.
      # Select only the rows from the output matrix that are also in the current matrix as well
      # as the columns in the output matrix that will represent the current matrix.
      # This selection of the output matrix can simply be overwritten by the current matrix.
      res_mat@data[rownames(res_mat@data) %in% rownames(cand@data),
                   cur_col:(cur_col + ncol(cand@data) - 1)] <- cand@data
      
      # Here we combine the support of the of the input frequent itemsets
      res_mat@support[cur_col:(cur_col + ncol(cand@data) - 1)] = cand@support
      
      # this 
      cur_col <- cur_col + ncol(cand@data)
    }
  }
  
  # Output the filled matrix.
  return(res_mat)
}

# ------------------------------------------------------------------------------------------------ #
# --------------------------------------- CombineFIMatrix----------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Combining list of Rules
#' 
#' This function takes as input a list of objects of class Rules that may have different items 
#' and rules and comines them to a single Rule.
#' @name CombineRules
#' @param list_input List of Rules. These Rules may have different number 
#' of rows, columns and number of items
#' @return Rules Object that will contain all the rules from the input rules objects.
CombineRules <- function(list_input) {
  
  # Determine the dimensions of the result matrix
  # It should have all rows, columns from all input matrices
  
  # Collect the number of itemsets / columns in ncols.
  ncols <- 0
  
  # collect the names of the different items in items
  items <- c()
  
  # Iterate over the list and add the number of columns as well as the items.
  for (cand in list_input) {
    ncols <- ncols + ncol(cand)
    items <- unique(c(items, items(cand)))
  }
  
  # Create the output matrix based on the dimension calculated above.
  # The matrix is created completely empty and will be filled later.
  res_mat <- new('Rules',
                 lhs = sparseMatrix(i = c(),
                                    j = c(),
                                    giveCsparse = FALSE,
                                    dims = c(length(items), ncols),
                                    dimnames = list(items, NULL)),
                 rhs = sparseMatrix(i = c(),
                                    j = c(),
                                    giveCsparse = FALSE,
                                    dims = c(length(items), ncols),
                                    dimnames = list(items, NULL)),
                 support = rep(0, ncols),
                 confidence = rep(0, ncols),
                 lift = rep(0, ncols),
                 leverage = rep(0, ncols),
                 itemsetID = rep(0, ncols),
                 FrequentItemsets = list_input[[1]]@FrequentItemsets)
  
  
  # Iterate again over the matrices in the input list and fill the output matrix with 
  # the correct entries from the matrices.
  
  # Initialize column counter that keeps track of which columns are currently overwritten
  # in the output matrix.
  cur_col <- 1
  
  for (cand in list_input) {
    if (ncol(cand) > 0) {
      
      # It is ensured that the current matrix is not empty.
      # Select only the rows from the output matrix that are also in the current matrix as well
      # as the columns in the output matrix that will represent the current matrix.
      # This selection of the output matrix can simply be overwritten by the current matrix.
      res_mat@lhs[items(res_mat) %in% rownames(cand@lhs),
                  cur_col:(cur_col + ncol(cand) - 1)] <- cand@lhs
      
      res_mat@rhs[items(res_mat) %in% rownames(cand@lhs),
                  cur_col:(cur_col + ncol(cand) - 1)] <- cand@rhs
      
      # Here we combine the support of the of the input frequent itemsets
      res_mat@support[cur_col:(cur_col + ncol(cand) - 1)] = cand@support
      
      # Here we combine the confidence of the of the input frequent itemsets
      res_mat@confidence[cur_col:(cur_col + ncol(cand) - 1)] = cand@confidence
      
      # Here we combine the lift of the of the input frequent itemsets
      res_mat@lift[cur_col:(cur_col + ncol(cand) - 1)] = cand@lift
      
      # Here we combine the leverage of the of the input frequent itemsets
      res_mat@leverage[cur_col:(cur_col + ncol(cand) - 1)] = cand@leverage
      
      # Here we combine the itemsetID of the of the input frequent itemsets
      res_mat@itemsetID[cur_col:(cur_col + ncol(cand) - 1)] = cand@itemsetID
      
      # this 
      cur_col <- cur_col + ncol(cand)
    }
  }
  
  # Output the filled matrix.
  return(res_mat)
}

# ------------------------------------------------------------------------------------------------ #
# ----------------------------------------- ExtractRules ----------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Extract Rules
#' 
#' This function extracts the rule from an object of  class Rules function in the form of 
#' {It1, ... ItN} -> {ITK}.
#' @name ExtractRules
#' @export
#' @param rules An object of class Rules.
#' @param maxNumConsequent Maximal length the reported rule consequent should have.
#' @param order_by Specifiy up to four metrics out of support, confidence, lift, leverage by which
#' the given rules should be sorted.
#' @param decreasing Should the rules start with the smallest or highest values of the specified
#' metrics?
#' @return The rules from the left hand and right hand side in the form of {It1, ... ItN} -> {ITK} 
#' in a data.frame. This data.frame does have columns lhs, rhs, unnamed, support and confidence.
ExtractRules <- function(rules, maxNumConsequent = NULL, order_by = NULL, decreasing = TRUE) {
  
  # For both the lhs and the rhs of the rules we select for each column the itemsets by names
  # that exist and combine them to vectors of strings where each string does reperesent one column
  # in the form {It1, It2, ..., Itn}. For the lhs this is the base itemset and for the rhs 
  # these are the consequents.
  lhs_rules <- apply(rules@lhs,2, function(col) {
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  rhs_rules <- apply(rules@rhs,2, function(col) {
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  # The colsums of the rhs do represent the number of items in each. We need this information 
  # to select only the rules that have fewer Items than maxNumConsequent.
  num_consequent <- colSums(rules@rhs)
  
  # Initialize the output dataset. This is a data.frame with variables lhs, REPLACE, rhs, 
  # support and confidence. The name REPLACE is only temporary and since this columns is always
  # filled with "=>" it does not have a name (for visual purposes). 
  out_data <- data.frame('lhs' = lhs_rules,
                         'REPLACE' = rep("=>", length(lhs_rules)),
                         rhs = rhs_rules,
                         Support = rules@support,
                         Confidence = rules@confidence,
                         Lift = rules@lift,
                         Leverage = rules@leverage)
  
  # Here the temporary "REPLACE" column name is replaced.
  colnames(out_data)[2] <- ""
  
  # If we do have a maxNumConsequent specified we will select only the rows from the dataframe 
  # that are of  consequent length < maxNumconsequent
  if (!is.null(maxNumConsequent)) {
    out_data <- out_data[num_consequent <= maxNumConsequent,]
  }
  
  # Here we order the dataframe containing the rules.
  # if in order_by no variables are given to order by we order the rules first by Support
  # and then by confidence
  
  if (is.null(order_by) || length(order_by) == 0) {
    out_data <- out_data[order(out_data$Support, out_data$Confidence, decreasing = decreasing),]
  } else {
    
    # if the order_by is provided we order the data.frame by the explicitly named columns in 
    # order_by
    
    # make all names columns lower case
    order_by <- tolower(order_by)
    
    # Only take unique elements
    order_by <- unique(order_by)
    
    # Only take elements that are support, confidence, lift or leverage
    order_by <- order_by[order_by %in% c('support', 'confidence', 'lift', 'leverage')]
    
    # If there are any order_by left then order by these columns
    if (length(order_by) != 0) {
      
      # Find the positions of the columns to order by 
      order_by <- unlist(lapply(order_by, function(x) {
        return(which(x == tolower(colnames(out_data))))
      }))
      
      if (length(order_by) == 1) {
        out_data <- out_data[order(out_data[,order_by[1], drop = FALSE], decreasing = decreasing),]
      } else {
        if (length(order_by) == 2) {
          out_data <- out_data <- out_data[order(out_data[,order_by[1], drop = FALSE],
                                                 out_data[,order_by[2], drop = FALSE],
                                                 decreasing = decreasing),]
        } else {
          if (length(order_by) == 3) {
            out_data <- out_data <- out_data[order(out_data[,order_by[1], drop = FALSE],
                                                   out_data[,order_by[2], drop = FALSE],
                                                   out_data[,order_by[3], drop = FALSE],
                                                   decreasing = decreasing),]
          } else {
            out_data <- out_data <- out_data[order(out_data[,order_by[1], drop = FALSE],
                                                   out_data[,order_by[2], drop = FALSE],
                                                   out_data[,order_by[3], drop = FALSE],
                                                   out_data[,order_by[4], drop = FALSE],
                                                   decreasing = decreasing),]
          }
        }
      }
      
    } else {
      warning('Please specify to columns to sort the rules by that are support, confidence,
              lift or leverage')
      }
    }
  
  # Since the data.frame was sorted the rownumbers are messed up and ugly, so they get replace 
  # by a nicer version. Also the row numbers do correspond to the rank of rules (based on first
  # their support, than their confidence).
  rownames(out_data) <- 1:nrow(out_data)
  
  # output the data.frame.
  return(out_data)
}

# ------------------------------------------------------------------------------------------------ #
# ------------------------------------- GiveUniqueCol -------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#' Return the unique columns of sparse matrix.
#' 
#' @name GiveUniqueCol
#' @param mat Sparse matrix of class ngTMatrix, lgCMatrix or ngCMatrix.
#' @export
#' @return Returns only non-duplicate columns of matrix. Ordering might be changed.
GiveUniqueCol <- function(mat) {
  
  # This fuction just relies on the Duplicate function that returns a boolean vector indicating
  # whether a certain column in the matrix is a duplicate. We only select non-duplicate columns
  return(mat[,!Duplicate(mat), drop = FALSE])
}