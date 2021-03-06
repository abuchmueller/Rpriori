% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoerceTypes.R
\name{makeFIMatrix.itemsets}
\alias{makeFIMatrix.itemsets}
\title{Function to create a FIMatrix from an itemsets}
\usage{
makeFIMatrix.itemsets(input, support, dataset)
}
\arguments{
\item{input}{Object of class itemsets}

\item{support}{Support values for the itemsets}

\item{dataset}{Underlying transactions. Has to be supplied if the support vector is missing and
has to be calculated.}
}
\value{
Object of class FIMatirx containing correct support values
}
\description{
Function to create a FIMatrix from an itemsets
}
