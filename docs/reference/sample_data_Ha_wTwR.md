# Sample cases under Ha for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

One latent genotype, ZD, for true donor (D) and one latent genotype, ZS,
for suspect (S).

## Usage

``` r
sample_data_Ha_wTwR(n, wT, wR, p, ...)
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
sample_data_Ha_wTwR(n = 10, wT = 0.3, wR = 1e-6, p = c(0.25, 0.25, 0.5))
#> $xT
#>       [,1]
#>  [1,]    1
#>  [2,]    1
#>  [3,]    1
#>  [4,]    0
#>  [5,]    1
#>  [6,]    0
#>  [7,]    1
#>  [8,]    0
#>  [9,]    0
#> [10,]    2
#> 
#> $xR
#>       [,1]
#>  [1,]    0
#>  [2,]    2
#>  [3,]    2
#>  [4,]    2
#>  [5,]    0
#>  [6,]    0
#>  [7,]    1
#>  [8,]    2
#>  [9,]    2
#> [10,]    1
#> 
sample_data_Ha_wTwR(n = 10, wT = 0.3, wR = 1e-6, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $xT
#>       [,1] [,2]
#>  [1,]    1    2
#>  [2,]    2    0
#>  [3,]    2    1
#>  [4,]    0    2
#>  [5,]    1    1
#>  [6,]    2    1
#>  [7,]    0    2
#>  [8,]    1    1
#>  [9,]    1    0
#> [10,]    1    2
#> 
#> $xR
#>       [,1] [,2]
#>  [1,]    1    1
#>  [2,]    0    1
#>  [3,]    2    1
#>  [4,]    2    1
#>  [5,]    1    1
#>  [6,]    0    1
#>  [7,]    2    1
#>  [8,]    1    1
#>  [9,]    2    1
#> [10,]    2    1
#> 
  
cases <- sample_data_Ha_wTwR(n = 1000, wT = 1e-1, wR = 1e-8, p = c(0.25, 0.25, 0.5))
tab <- table(xT = cases$xT, xR = cases$xR)
tab
#>    xR
#> xT    0   1   2
#>   0  74  60 111
#>   1  79 108 158
#>   2 114 106 190

cases <- sample_data_Ha_wTwR(n = 1000, wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
tab <- table(xT = cases$xT, xR = cases$xR)
tab
#>    xR
#> xT    0   1   2
#>   0  64  68 117
#>   1  65  63 137
#>   2 116 125 245
```
