# here i will test the create of length 2 candidates #


### TESTING ####


# for five elements #
n <- 5
L1 <- diag(rep(TRUE, n))

cols_need <- sum((1:(nrow(L1) - 1)))
cols_need

L2 <- matrix(rep(FALSE, nrow(L1) * cols_need), nrow = nrow(L1))
rownames(L2) <- as.character(1:n)
cols <- 0:(cols_need - 1) * nrow(L1)

pos <- c(rep(1:(nrow(L1) - 1), times = (nrow(L1) - 1):1 ) + cols, unlist(lapply(2:nrow(L1), seq, to = nrow(L1))) + cols)

L2[pos] <- TRUE

all(combn(as.character(1:n), 2) == apply(L2, 2, function(x){
  return(rownames(L2)[x])
}))