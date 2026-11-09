# Sample cases under Hd for sample-dependent error probabilities, \$w_D\$ and \$w_S\$

One latent genotype, ZD, for true donor (D) and one latent genotype, ZS,
for suspect (S).

## Usage

``` r
sample_data_Hd_wDwS(n, wD, wS, p, ...)
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
sample_data_Hd_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#> $X_D
#>       [,1]
#>  [1,]    1
#>  [2,]    1
#>  [3,]    0
#>  [4,]    2
#>  [5,]    2
#>  [6,]    1
#>  [7,]    2
#>  [8,]    1
#>  [9,]    0
#> [10,]    1
#> 
#> $X_S
#>       [,1]
#>  [1,]    0
#>  [2,]    2
#>  [3,]    2
#>  [4,]    1
#>  [5,]    2
#>  [6,]    0
#>  [7,]    0
#>  [8,]    2
#>  [9,]    2
#> [10,]    1
#> 
sample_data_Hd_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $X_D
#>       [,1] [,2]
#>  [1,]    2    1
#>  [2,]    1    1
#>  [3,]    1    1
#>  [4,]    0    0
#>  [5,]    0    1
#>  [6,]    0    2
#>  [7,]    1    0
#>  [8,]    0    0
#>  [9,]    1    1
#> [10,]    2    1
#> 
#> $X_S
#>       [,1] [,2]
#>  [1,]    0    1
#>  [2,]    2    1
#>  [3,]    0    1
#>  [4,]    0    1
#>  [5,]    2    1
#>  [6,]    1    1
#>  [7,]    0    1
#>  [8,]    1    0
#>  [9,]    2    1
#> [10,]    2    1
#> 
  
cases <- sample_data_Hd_wDwS(n = 1000, wD = 1e-1, wS = 1e-8, p = c(0.25, 0.25, 0.5))
tab <- table(X_D = cases$X_D, X_S = cases$X_S)
tab
#>    X_S
#> X_D   0   1   2
#>   0  59  47 103
#>   1  81 101 163
#>   2 103 119 224

cases <- sample_data_Hd_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
tab <- table(X_D = cases$X_D, X_S = cases$X_S)
tab
#>    X_S
#> X_D   0   1   2
#>   0  55  65 136
#>   1  60  73 131
#>   2 120 125 235
```
