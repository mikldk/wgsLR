# Sample cases under Hp for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

Same latent genotype, Z, with independent errors for true donor (D) and
suspect (S).

## Usage

``` r
sample_data_Hp_wTwR(n, wT, wR, p, ...)
```

## Arguments

- n:

  number of samples

- wT:

  error probability for donor sample

- wR:

  error probability for PoI sample

- p:

  list of genotype probabilities (length is number of loci) or vector of
  length 3 for single locus

- ...:

  Passed on to [`add_errors_to_genotypes()`](add_errors_to_genotypes.md)

## Value

list of two matrices, each of size n x loci with genotype in 0/1/2
format resembling the situation in real life.

## Examples

``` r
sample_data_Hp_wTwR(n = 10, wT = 0.3, wR = 1e-6, p = c(0.25, 0.25, 0.5))
#> $xT
#>       [,1]
#>  [1,]    1
#>  [2,]    2
#>  [3,]    2
#>  [4,]    1
#>  [5,]    2
#>  [6,]    1
#>  [7,]    0
#>  [8,]    1
#>  [9,]    1
#> [10,]    2
#> 
#> $xR
#>       [,1]
#>  [1,]    1
#>  [2,]    2
#>  [3,]    2
#>  [4,]    0
#>  [5,]    2
#>  [6,]    2
#>  [7,]    0
#>  [8,]    0
#>  [9,]    1
#> [10,]    2
#> 
sample_data_Hp_wTwR(n = 10, wT = 0.3, wR = 1e-6, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $xT
#>       [,1] [,2]
#>  [1,]    1    2
#>  [2,]    1    1
#>  [3,]    1    1
#>  [4,]    1    1
#>  [5,]    1    0
#>  [6,]    2    1
#>  [7,]    1    1
#>  [8,]    1    1
#>  [9,]    1    1
#> [10,]    1    2
#> 
#> $xR
#>       [,1] [,2]
#>  [1,]    2    1
#>  [2,]    2    1
#>  [3,]    1    1
#>  [4,]    1    1
#>  [5,]    1    1
#>  [6,]    2    1
#>  [7,]    0    1
#>  [8,]    2    1
#>  [9,]    0    1
#> [10,]    2    2
#> 
  
cases <- sample_data_Hp_wTwR(n = 1000, wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
table(xT = cases$xT, xR = cases$xR)
#>    xR
#> xT    0   1   2
#>   0 257   0   0
#>   1   0 262   0
#>   2   0   0 481
cases <- sample_data_Hp_wTwR(n = 1000, wT = 0.1, wR = 0, p = c(0.25, 0.25, 0.5))
table(xT = cases$xT, xR = cases$xR)
#>    xR
#> xT    0   1   2
#>   0 211  22   8
#>   1  43 206  85
#>   2   0  22 403
cases <- sample_data_Hp_wTwR(n = 1000, wT = 0, wR = 0.1, p = c(0.25, 0.25, 0.5))
table(xT = cases$xT, xR = cases$xR)
#>    xR
#> xT    0   1   2
#>   0 194  52   3
#>   1  23 210  21
#>   2   6  81 410
  
cases <- sample_data_Hp_wTwR(n = 1000, wT = 1e-1, wR = 1e-8, p = c(0.25, 0.25, 0.5))
tab <- table(xT = cases$xT, xR = cases$xR)
tab
#>    xR
#> xT    0   1   2
#>   0 199  19   4
#>   1  49 208 100
#>   2   1  15 405
estimate_w(tab)
#> [1] 0.05371926

cases <- sample_data_Hp_wTwR(n = 1000, wT = 0, wR = 0, p = c(0.1, 0.7, 0.2))
tab <- table(xT = cases$xT, xR = cases$xR)
diag(tab/sum(tab))
#>     0     1     2 
#> 0.115 0.702 0.183 
```
