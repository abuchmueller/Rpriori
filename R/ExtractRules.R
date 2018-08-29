#' Extract Rules
#' 
#' This function extracts the rule from the output of Rapriori Association rules
#' function in the form of {It1, ... ItN} -> {ITK}.
#' @name ExtractRules
#' @export
#' @param rules A output object from Rapriori's Association rules function has to be used
#' here.
#' @param maxNumConsequent The maximum length of consequents that the rules of the ouput should 
#' have. In Default all rules are shown.
#' @param order_by Specifiy up to four metrics out of support, confidence, lift, leverage by which
#' the given rules should be sorted. The first one used first and son on. 
#' @param decreasing Should the rules start with the smallest or highest values of the specified
#' metrics?
#' @return The rules from the left hand and right hand side in the form of {It1, ... ItN} -> {ITK} 
#' in a data.frame. This data.frame does have columns lhs, rhs, unnamed, support and confidence.

ExtractRules <- function(rules, maxNumConsequent = NULL, order_by = NULL, decreasing = TRUE){
      
  # For both the lhs and the rhs of the rules we select for each column the itemsets by names
  # that exist and combine them to vectors of strings where each string does reperesent one column
  # in the form {It1, It2, ..., Itn}. For the lhs this is the base itemset and for the rhs 
  # these are the consequents.
  lhs_rules <- apply(rules@lhs,2, function(col){
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  rhs_rules <- apply(rules@rhs,2, function(col){
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
  if (!is.null(maxNumConsequent)){
    out_data <- out_data[num_consequent <= maxNumConsequent,]
  }
  
  # Here we order the dataframe containing the rules.
  # if in order_by no variables are given to order by we order the rules first by Support
  # and then by confidence
  
  if (is.null(order_by) || length(order_by) == 0){
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
    if (length(order_by) != 0){
      
      # Find the positions of the columns to order by 
      order_by <- unlist(lapply(order_by, function(x) {
        return(which(x == tolower(colnames(out_data))))
      }))
      
      if (length(order_by) == 1){
        out_data <- out_data[order(out_data[,order_by[1], drop = FALSE], decreasing = decreasing),]
      } else {
        if (length(order_by) == 2){
          out_data <- out_data <- out_data[order(out_data[,order_by[1], drop = FALSE],
                                                 out_data[,order_by[2], drop = FALSE],
                                                 decreasing = decreasing),]
        } else {
          if (length(order_by) == 3){
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





