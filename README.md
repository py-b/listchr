
<!-- README.md is generated from README.Rmd. Please edit that file -->
listchr
=======

The goal of *listchr* is to flatten list-columns of a data frame into strings, in order to export under a more simple format (csv, ...).

The reverse transformation (separated string to list) can also be done.

Installation
------------

You can install *listchr* from github with :

``` r
# install.packages("devtools")
devtools::install_github("the-knife/listchr")
```

Example
-------

Convert all list-columns of a data frame into character vectors :

``` r
library(listchr)

df <- data.frame(x = 1:2)
df$y <- list(letters[1:3], 1:2)
df$z <- list(list(3:4, 5:7), "d")

str(df)
#> 'data.frame':    2 obs. of  3 variables:
#>  $ x: int  1 2
#>  $ y:List of 2
#>   ..$ : chr  "a" "b" "c"
#>   ..$ : int  1 2
#>  $ z:List of 2
#>   ..$ :List of 2
#>   .. ..$ : int  3 4
#>   .. ..$ : int  5 6 7
#>   ..$ : chr "d"

df2 <- listcol_to_chr(df)
str(df2)
#> 'data.frame':    2 obs. of  3 variables:
#>  $ x: int  1 2
#>  $ y: chr  "a;b;c" "1;2"
#>  $ z: chr  "3;4|5;6;7" "d"
```
