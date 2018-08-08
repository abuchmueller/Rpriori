#' Extract Rules
#' 
#' This function extracts the rule from the output of ProjectAprioris Association rules
#' function in the form of {It1, ... ItN} -> {ITK}.
#' @name ExtractRules
#' @export
#' @param rules A output object from Project_Apriori's Association rules function has to be used
#' here.
#' @param maxNumConsequent The maximum length of consequents that the rules of the ouput should 
#' have. In Default all rules are shown.
#' @return The rules from the left hand and right hand side in the form of {It1, ... ItN} -> {ITK} 
#' in a data.frame. This data.frame does have columns lhs, rhs, unnamed, support and confidence.

ExtractRules <- function(rules, maxNumConsequent = NULL){
  
  # For both the lhs and the rhs of the rules we select for each column the itemsets by names
  # that exist and combine them to vectors of strings where each string does reperesent one column
  # in the form {It1, It2, ..., Itn}. For the lhs this is the base itemset and for the rhs 
  # these are the consequents.
  lhs_rules <- apply(rules$lhs,2, function(col){
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  rhs_rules <- apply(rules$rhs,2, function(col){
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  # The colsums of the rhs do represent the number of items in each. We need this information 
  # to select only the rules that have fewer Items than maxNumConsequent.
  num_consequent <- colSums(rules$rhs)
  
  # Initialize the output dataset. This is a data.frame with variables lhs, REPLACE, rhs, 
  # support and confidence. The name REPLACE is only temporary and since this columns is always
  # filled with "=>" it does not have a name (for visual purposes). 
  out_data <- data.frame('lhs' = lhs_rules,
                         'REPLACE' = rep("=>", length(lhs_rules)),
                         rhs = rhs_rules,
                         Support = rules$supp,
                         Confidence = rules$conf)
  
  # Here the temporary "REPLACE" column name is replaced.
  colnames(out_data)[2] <- ""
  
  # If we do have a maxNumConsequent specified we will select only the rows from the dataframe 
  # that are of  consequent length < maxNumconsequent
  if (!is.null(maxNumConsequent)){
    out_data <- out_data[num_consequent <= maxNumConsequent,]
  }
  
  # We sort the outputed rules first by support than by confidence since this does represent
  # their importance.
  out_data <- out_data[order(out_data$Support, out_data$Confidence, decreasing = TRUE),]
  
  # Since the data.frame was sorted the rownumbers are messed up and ugly, so they get replace 
  # by a nicer version. Also the row numbers do correspond to the rank of rules (based on first
  # their support, than their confidence).
  rownames(out_data) <- 1:nrow(out_data)
  
  # output the data.frame.
  return(out_data)
}


