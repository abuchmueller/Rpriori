#' Extract Rules
#' 
#' This function extracts the rule in the form of {It1, ... ItN} -> {ITK}.
#' @name ExtractRules
#' @export
#' @param rules The left hand side and the right hand side of the rules incident matrix where the names rows do represent the items. The both matrices should
#' be saved in a list.
#' @return The rules from the left hand and right hand side in the form of {It1, ... ItN} -> {ITK}.

ExtractRules <- function(rules, maxNumConsequent = NULL){
  
  lhs_rules <- apply(rules$lhs,2, function(col){
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  rhs_rules <- apply(rules$rhs,2, function(col){
    paste("{", paste(names(col)[col], collapse = ", "), "}", sep = "")
  }) 
  
  num_consequent <- apply(rules$rhs, 2,sum)
  
  out_data <- data.frame('lhs' = lhs_rules, 'REPLACE' = rep("=>", length(lhs_rules)), rhs = rhs_rules, Support = rules$supp, Confidence = rules$conf)
  colnames(out_data)[2] <- ""
  if (!is.null(maxNumConsequent)){

    out_data <- out_data[num_consequent <= maxNumConsequent,]
  }
  
  out_data <- out_data[order(out_data$Support, out_data$Confidence, decreasing = TRUE),]
  
  rownames(out_data) <- 1:nrow(out_data)
  
  return(out_data)
}


