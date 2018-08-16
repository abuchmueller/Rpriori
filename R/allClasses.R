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

#setValidity("TAMatrix", "initialize"))

#setMethod("intitialize", "TAMatrix", function(.Object) {
  
#})

setClass("TAMatrix2", 
         slots = list(
           data = "ngTMatrix", 
           dim = "integer", 
           items = "character")
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

#setMethod("initialize", signature())

#' An S4 class to represent an frequent Itemsets in a sparse Matrix
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
           confidence = "numeric"
         )
)


