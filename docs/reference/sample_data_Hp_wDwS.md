# Sample cases under Hp for sample-dependent error probabilities, \$w_D\$ and \$w_S\$

Same latent genotype, Z, with independent errors for true donor (D) and
suspect (S).

## Usage

``` r
sample_data_Hp_wDwS(n, wD, wS, p, ...)
```

## Arguments

- n:

  number of samples

- wD:

  error probability for donor sample

- wS:

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
sample_data_Hp_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#> $X_D
#>       [,1]
#>  [1,]    1
#>  [2,]    1
#>  [3,]    1
#>  [4,]    0
#>  [5,]    1
#>  [6,]    1
#>  [7,]    2
#>  [8,]    1
#>  [9,]    1
#> [10,]    1
#> 
#> $X_S
#>       [,1]
#>  [1,]    2
#>  [2,]    1
#>  [3,]    1
#>  [4,]    1
#>  [5,]    0
#>  [6,]    2
#>  [7,]    2
#>  [8,]    2
#>  [9,]    0
#> [10,]    0
#> 
sample_data_Hp_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $X_D
#>       [,1] [,2]
#>  [1,]    2    2
#>  [2,]    1    0
#>  [3,]    0    0
#>  [4,]    1    1
#>  [5,]    1    1
#>  [6,]    2    1
#>  [7,]    1    1
#>  [8,]    1    1
#>  [9,]    2    0
#> [10,]    2    1
#> 
#> $X_S
#>       [,1] [,2]
#>  [1,]    2    1
#>  [2,]    2    1
#>  [3,]    0    1
#>  [4,]    0    1
#>  [5,]    1    1
#>  [6,]    2    1
#>  [7,]    2    1
#>  [8,]    2    1
#>  [9,]    2    1
#> [10,]    2    1
#> 
  
cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
table(X_D = cases$X_D, X_S = cases$X_S)
#>    X_S
#> X_D   0   1   2
#>   0 238   0   0
#>   1   0 244   0
#>   2   0   0 518
cases <- sample_data_Hp_wDwS(n = 1000, wD = 0.1, wS = 0, p = c(0.25, 0.25, 0.5))
table(X_D = cases$X_D, X_S = cases$X_S)
#>    X_S
#> X_D   0   1   2
#>   0 218  20   3
#>   1  39 224  85
#>   2   4  15 392
cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0.1, p = c(0.25, 0.25, 0.5))
table(X_D = cases$X_D, X_S = cases$X_S)
#>    X_S
#> X_D   0   1   2
#>   0 192  47   3
#>   1  16 216  23
#>   2   7 100 396
  
cases <- sample_data_Hp_wDwS(n = 1000, wD = 1e-1, wS = 1e-8, p = c(0.25, 0.25, 0.5))
tab <- table(X_D = cases$X_D, X_S = cases$X_S)
tab
#>    X_S
#> X_D   0   1   2
#>   0 201  23   4
#>   1  42 184  93
#>   2   0  26 427
estimate_w(tab)
#> [1] 0.05340395

cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.1, 0.7, 0.2))
tab <- table(X_D = cases$X_D, X_S = cases$X_S)
diag(tab/sum(tab))
#>     0     1     2 
#> 0.088 0.693 0.219 
```
