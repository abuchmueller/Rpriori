#' Generate Candidate of length k + 1 based on frequent itemsets of length k.
#' 
#' Take an sparce, binary, incident matrix of class ngTMatrix with itemsets of length k and
#' generates the candidates of length  k + 1 based on the Apriori Candidate Generation. In the
#' matrix the rows should be the items and the columns should be the itemssets that represent 
#' the candidates.
#' @name GenCandidates
#' @export
#' @param L Sparse, binary  incident matrix of class ngTMatrix containing the candidates itemsets.
#' The rows should be named and represent the items while the columns should be the itemsets. 
#' @return The candidates of length k + 1 as a sparse incident matrix. rownames are kept.
#' @importFrom utils combn
GenCandidates <- function(L){
  
  # We assume later on that L does only have unique columns. Here we do that.
  L <- GiveUniqueCol(L)
  
  # K does store the number of items in the current Itemsets. We assume that in L
  # there are only itemsets of a certain length since this does coincide with the 
  #logic of apriori both in the frequent Itemset as well as rule generation.
  
  if (ncol(L) > 0) {
    K <- sum(L[,1])
  } else {
    return(L)
  }
  
  # ------------------------------------------------- #
  # Step 1: Joining Itemsets with k-1 Items in common #
  # --------------------------------------------------#
  
  # Here we have to combine all two itemsets that have at least K common itemsets.
  # First find all possible combination of two columns from the input matrix.
  poss_combs <- combn(1:ncol(L), 2)
  
  # The resulting matrix poss_comps has two rows. For each column the first row does desribe the
  # column of the input matrix that has to be combined with the column described in the second row. 
  # Therefore, I will first of all create the matrix that results from the selecing the columns of
  # the input matrix named in the first row, then the one that results from selecting the columns
  # specified in the second row of poss_combs.
  
  First_select <- L[,poss_combs[1,], drop = FALSE]
  Second_select <- L[,poss_combs[2,], drop = FALSE]
  
  # Finally the combination of the columns selected in row one and two is done by or operator 
  # since it selectes every item that is either in the first or second matrix.
  cand <- First_select | Second_select
  
  # So far we found and saved all possible combinations of two columns from the input matrix in 
  # cand. Now we have to insure that we only select the created columns that come from the columns
  # that had k - 1 items in common. If we combine two columns that each have K items and they have
  # K - 1 items in common their combination must have exactly K + 1 items. Therefore, delete all
  # columns from cand that do not have K + 1 items
  cand <- cand[,colSums(cand) == K + 1, drop = FALSE]
  
  # Also delete all the created dublicates.
  cand <- GiveUniqueCol(cand)
  
  # ----------------------------------------------------------------------------------- #
  # Step 2: Prune out candidates that contain subset of length k that are not frequent. #
  # ----------------------------------------------------------------------------------- #
  
  # In the last step we generated potentially many frequent itemsets for which have to determine 
  # the support later on to make sure that they are frequent. Yet we can exlude some itemsets
  # based on the apriori property which states that all subsets of frequent itemsets have
  # to be frequent. When we find any subsets of lenght k of a candidate that is not frequent
  # than we can exlude that candidate.
  
  # When I multiply the transposed of the candidate set with the original set I can get a 
  # maximum of k matches since there are only K items in the itemsets from L. Now all
  # Subsets of length K of the candidates must be frequent  itemsets so that the candidate might
  # be a frequent itemset. Therefore for each candidate multiplied with the L we have to get K + 1
  # complete matches. A complete match is when the candidate multiplied with on itemset from L with
  # K elements results in the value k.
  
  # matrix * 1 make the boolean matrix numeric.
  select <- rowSums(t(cand * 1) %*% (L * 1) == K) == K + 1
  
  # Return the candidate matrix and exclude the candidates base on the logic described above.
  cand <- cand[, select, drop = FALSE]
  
  # Using the matrix functions from above, the matrix was defined as Compressed. but our logic
  # needs a non-compressed matrix. Therefore, we create a new uncompressed sparse matrix based
  # on the matrix created above.
  
  cand <- sparseMatrix(i = cand@i,
                    p = cand@p,
                    giveCsparse = FALSE,
                    index1 = FALSE,
                    dims = c(nrow(cand), ncol(cand)),
                    dimnames = list(rownames(cand), NULL))
  
  return(cand)
}