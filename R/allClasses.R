#### This file stores all classes (Itemsets, FrequentItems & Rules) ####
########################################################################

#' An S4 class to represent an Itemset as a Sparse Matrix
#' @name TAMatrix
#' @rdname Transactionmatrix-class
#' @slot data Transaction Matrix in sparse representation
#' @exportClass TAMatrix
setClass("TAMatrix",
        representation(
           data  = "ngTMatrix", 
           dim  = "integer",
           items = "character"
         )
       #lacks validity checking
)


#' An S4 class to represent an frequent Itemsets in a sparse Matrix
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


#' An S4 class to represent association rules and additional information
#' @name Rules
#' @rdname Rules-class  
#' @slot lhs Left-hand side 
#' @slot rhs Right-hande side
#' @slot support Support vector of Rules
#' @slot confidence Confidence vector of Rules
#' @exportClass Rules
setClass("Rules",
         representation(
           lhs = "ngTMatrix",
           rhs = "ngTMatrix",
           support = "numeric",
           confidence = "numeric",
           lift = "ANY",
           leverage = "ANY",
           itemsetID = "ANY",
           FrequentItemsets = "ANY"
         ),
         validity = function(object) {
           stopifnot(length(object@rhs)==length(object@lhs)) 
         }
)


