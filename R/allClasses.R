#### This file stores all classes (Itemsets, FrequentItems & Rules) ####
########################################################################

#' An S4 class to represent an itemset as a sparse Matrix
#' @name TAMatrix
#' @rdname Transactionmatrix-class
#' @slot data Transaction Matrix in binary sparse representation
#' @exportClass TAMatrix
setClass("TAMatrix",
        representation(
           data  = "ngTMatrix", 
           dim  = "integer",
           items = "character"
         )
)


#' An S4 class to represent frequent itemsets in a sparse Matrix
#' @name FIMatrix
#' @rdname Frequentitems-class
#' @slot data Frequent Itemset Matrix in sparse representation
#' @slot support Corresponding support
#' @exportClass FIMatrix
setClass("FIMatrix", 
         representation(
           data = "ngTMatrix",
           support = "numeric")
)


#' An S4 class to represent association rules and metrics
#' @name Rules
#' @rdname Rules-class  
#' @slot lhs Left-hand side 
#' @slot rhs Right-hande side
#' @slot support Support vector of Rules
#' @slot confidence Confidence vector of Rules
#' @slot lift Lift vector
#' @slot leverage Leverage vector
#' @exportClass Rules
setClass("Rules",
         representation(
           lhs = "ngTMatrix",
           rhs = "ngTMatrix",
           support = "numeric",
           confidence = "numeric",
           lift = "numeric",
           leverage = "numeric",
           itemsetID = "numeric",
           FrequentItemsets = "FIMatrix"
         ),
         validity = function(object) {
           stopifnot(length(object@rhs)==length(object@lhs)) 
         }
)


