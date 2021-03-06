% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allMethods.R
\docType{methods}
\name{select-FIMatrix}
\alias{select-FIMatrix}
\alias{select,FIMatrix-method}
\title{Subsetting of an FIMatrix}
\usage{
\S4method{select}{FIMatrix}(x, i, j)
}
\arguments{
\item{x}{Object of class FIMatrix.}

\item{i}{Either the rows represented by their row number or a logical vector of length number of 
row of FIMAtrix. If missing or NULL all rows are selected.}

\item{j}{Either the columns represented by their columns numbers or logical vector of length 
number of columns in FIMatrix. If missing or NULL all columns are selected.}
}
\value{
subsetted FIMatrix
}
\description{
An FImatrix does contain the matrix of itemsets as well as the a vector that contains the support
for each itemset. Therefore, both are logically connected and when a FIMatrix is subsetted column-
wise the supported vector has to be subsetted as well.
}
