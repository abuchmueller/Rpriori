% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allMethods.R
\docType{methods}
\name{rowSums-TAMatrix}
\alias{rowSums-TAMatrix}
\alias{rowSums,TAMatrix-method}
\title{Return the row Sums for the underlying matrix within an TAMatrix.}
\usage{
\S4method{rowSums}{TAMatrix}(x)
}
\arguments{
\item{x}{Object of class TAMatrix}
}
\value{
numeric vector containing the sum of each row of the TAMatrix
}
\description{
In the matrix underlying the TAMatrix the rows represent the items and the columns to represent 
the itemsets. Here the sums of each row should be calculated that are the number of occurences
of each item in the different transactions.
}
