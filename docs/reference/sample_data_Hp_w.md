# Sample cases under Hp for one error probability, \$w\$

Same latent genotype, Z, with independent errors for true trace donor
(xT) and reference donor (xR).

## Usage

``` r
sample_data_Hp_w(n, w, p, ...)
```

## Arguments

- n:

  number of samples

- w:

  error probability

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
sample_data_Hp_w(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#> $xT
#>       [,1]
#>  [1,]    1
#>  [2,]    2
#>  [3,]    0
#>  [4,]    1
#>  [5,]    2
#>  [6,]    1
#>  [7,]    1
#>  [8,]    2
#>  [9,]    1
#> [10,]    2
#> 
#> $xR
#>       [,1]
#>  [1,]    1
#>  [2,]    1
#>  [3,]    0
#>  [4,]    2
#>  [5,]    2
#>  [6,]    0
#>  [7,]    2
#>  [8,]    2
#>  [9,]    1
#> [10,]    1
#> 
sample_data_Hp_w(n = 10, w = 0.1, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $xT
#>       [,1] [,2]
#>  [1,]    0    1
#>  [2,]    2    1
#>  [3,]    2    1
#>  [4,]    2    1
#>  [5,]    2    1
#>  [6,]    1    1
#>  [7,]    1    1
#>  [8,]    1    1
#>  [9,]    0    1
#> [10,]    0    1
#> 
#> $xR
#>       [,1] [,2]
#>  [1,]    0    1
#>  [2,]    1    1
#>  [3,]    2    1
#>  [4,]    2    1
#>  [5,]    1    2
#>  [6,]    1    1
#>  [7,]    2    1
#>  [8,]    1    2
#>  [9,]    0    1
#> [10,]    1    1
#> 
  
cases <- sample_data_Hp_w(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
tab <- table(xT = cases$xT, xR = cases$xR)
tab
#>    xR
#> xT    0   1   2
#>   0 100  77  40
#>   1 104 212 144
#>   2  51 155 117
estimate_w(tab)
#> [1] 0.3003395

cases <- sample_data_Hp_w(n = 1000, w = 0, p = c(0.1, 0.7, 0.2))
tab <- table(xT = cases$xT, xR = cases$xR)
diag(tab/sum(tab))
#>     0     1     2 
#> 0.095 0.692 0.213 
```
