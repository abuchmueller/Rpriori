
<!-- README.md is generated from README.Rmd. Please edit that file -->
ProjectApriori
==============

The goal of ProjectApriori is to ...

Installation
------------

You can install ProjectApriori from github with:

``` r
# install.packages("devtools")
devtools::install_github("TimToebrock/Project_Apriori")
```

Example
-------

This is a basic example which shows you how to create association rules with Rpriori using the "Groceries" dataset:

``` r
data("Groceries")
Rules <- AssociationRules(Itemsets=Groceries, minsupport = 0.01, minconfidence = 0) 
#> Loading required package: arules
#> Loading required package: Matrix
#> 
#> Attaching package: 'arules'
#> The following objects are masked from 'package:base':
#> 
#>     abbreviate, write
```

To create rules you need to specifiy an transactions databse and a minimum support threshold but you can also set a minimum confidence threshold.

Inspecting the data
-------------------

There are multiple ways to inspect the data used to create the rules:

``` r
Frequent <- extract(Rules)
summary(Frequent)
#> 
#> Frequent itemsets in binary sparse matrix representation 
#>  with 88 rows (items) and 
#>  333 columns (frequent itemsets)
#> 
#> Most frequent items: 
#>       whole milk other vegetables           yogurt       rolls/buns 
#>               71               63               39               36 
#>  root vegetables             soda   tropical fruit          sausage 
#>               34               28               26               17 
#> 
#> 
#> Observed frequency in frequent itemsets:
#>       whole milk other vegetables           yogurt       rolls/buns 
#>           0.2132           0.1892           0.1171           0.1081 
#>  root vegetables             soda   tropical fruit          sausage 
#>           0.1021           0.0841           0.0781           0.0511 
#> 
#> 
#> Distribution of itemset length:
#> 
#>   1   2   3 
#>  88 213  32 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   1.000   2.000   1.832   2.000   3.000 
#> 
#> 
#> Summary of the support measure:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.01007 0.01190 0.01627 0.02507 0.02603 0.25552
```
