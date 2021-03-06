% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoerceTypes.R
\name{outputFIMatrix}
\alias{outputFIMatrix}
\title{Function to create a FIMatrix from an ngTMatrix as well as the corresponding vector of support.}
\usage{
outputFIMatrix(mat, support)
}
\arguments{
\item{mat}{Object of class ngTMatrix}

\item{support}{Vector containing the coresponding support values for the frequent itemsets in
mat}
}
\value{
Object of class FIMatrix corresponding to the ngTMatrix and the support vector
}
\description{
Function to create a FIMatrix from an ngTMatrix as well as the corresponding vector of support.
}
