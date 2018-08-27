#### This file stores all classes ####
######################################

#' S4 class to represent transactions in a sparse Matrix
#' @name TAMatrix-class
#' @rdname TAMatrix-class
#' @slot data Transaction Matrix in binary sparse representation
#' @exportClass TAMatrix
setClass("TAMatrix",
        representation(
           data  = "ngTMatrix", 
           dim  = "integer",
           items = "character"
         ),
        
        validity = function(object) {
           
           # There should be a column and row dimension of the data.
           if (length(object@dim) != 2){
             stop("Invalid defintion of TAMatrx: dim should contain the number of rows and columns")
           }
           
           # The first value of dim should describe the number of rows the second one the number
           # of columns
           if(object@dim[1] != nrow(object@data)){
             stop("Invalid defintion of TAMatrx: Wrong number of rows")
           }
           
           if(object@dim[2] != ncol(object@data)){
             stop("Invalid defintion of TAMatrx: Wrong number of columns")
           }
           
           # in items the names of the items should be stored, therefore it must have as many
           # elements as the matrix has rows.
           if(length(object@items) != nrow(object@data)){
             stop("Invalid defintion of TAMatrx: Wrong number items in items")
           }
           
         }
)


#' S4 class to represent frequent itemsets in a sparse Matrix
#' @name FIMatrix-class
#' @rdname FIMatrix-class
#' @slot data Frequent Itemset Matrix in sparse representation. Rows are the items and the 
#' columns represent the itemsets.
#' @slot support Vector containing the support of all Itemsets
#' @exportClass FIMatrix
setClass("FIMatrix", 
         representation(
           data = "ngTMatrix",
           support = "numeric"
          ),
         
         validity = function(object) {
           
           # here validity checking for the FIMatrix class is done.
           
           # For each columns (itemset) in the matrix there should be one corresponding
           # support values in support.
           if(ncol(object@data) != length(object@support)){
             stop("For each itemset there should be the corresponding support saved in slot support.")
           } 
           
           # The support is the relative frequency of the itemset and can therefore only be within 
           # the interval (0,1)
           if (any(object@support > 1) || any(object@support < 0)){
             stop("The support can only be within (0,1)")
           }
        }
)


#' S4 class to store association rules and relevant quality metrics
#' @name Rules-class
#' @rdname Rules-class  
#' @slot lhs ngTMatrix describing the lhs of the rules
#' @slot rhs ngTMatrix describing the rhs of the rules
#' @slot support Support of  the Rules
#' @slot confidence Confidence of Rules
#' @slot lift Lift of the rules
#' @slot leverage Leverage of the rules
#' @slot ItemsetID This represent a vector of unique identifiers for the different frequent
#'  itemsets. Used for internal computation.
#' @slot FrequentItemsets Internally used FIMatrix containing the frequent itemsets based on which
#' the rules were created.
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
           
           # here validity checking for the rules class is done.
           
           # A rule consists of the i'th columns of rhs and lhs. Therefore, both of 
           # them must have the same number of columns
           if(ncol(object@rhs) != ncol(object@lhs)){
             stop("The lhs should have as many items / columns as the right hand side")
           } 
           
           # In support, confidence, lift, leverage and itemsetID additional information
           # about each of the rules is saved. Therefore, for each column (rule) in lhs and
           # rhs there hat to be exactly one element in support, confidence, lift, leverage and
           # itemsetID
           if(ncol(object@rhs) != length(object@support)){
             stop("For each of the items in the rules there should be a corresponding support
                  value.")
           }
           
           if(ncol(object@rhs) != length(object@confidence)){
             stop("For each of the items in the rules there should be a corresponding confidence
                  value.")
           }
           
           if(ncol(object@rhs) != length(object@lift)){
             stop("For each of the items in the rules there should be a corresponding lift
                  value.")
           }
           
           if(ncol(object@rhs) != length(object@leverage)){
             stop("For each of the items in the rules there should be a corresponding leverage
                  value.")
           }
           
           if(ncol(object@rhs) != length(object@itemsetID)){
             stop("For each of the items in the rules there should be a corresponding itemsetID
                  value.")
           }
           
           # The support is the relative frequency of the itemset and can therefore only be within 
           # the interval (0,1)
           if (any(object@support > 1) || any(object@support < 0)){
             stop("The support can only be within (0,1)")
           }
           
           # The confidence is (basicly) a relative frequency of the itemset and can therefore
           # only be within the interval (0,1)
           if (any(object@confidence > 1) || any(object@confidence < 0)){
             stop("The confidence can only be within (0,1)")
           }
         }
)

