# Sample cases under Hp

Same latent genotype, Z, with independent errors for true donor (D) and
suspect (S).

## Usage

``` r
sample_data_Hp(n, w, p)
```

## Arguments

- n:

  number of samples

- w:

  error probability

- p:

  list of genotype probabilities (length is number of loci) or vector of
  length 3 for single locus

## Value

list of two matrices, each of size n x loci with genotype in 0/1/2
format resembling the situation in real life.

## Examples

``` r
sample_data_Hp(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#> $X_D
#>       [,1]
#>  [1,]    2
#>  [2,]    1
#>  [3,]    1
#>  [4,]    0
#>  [5,]    0
#>  [6,]    2
#>  [7,]    2
#>  [8,]    2
#>  [9,]    2
#> [10,]    1
#> 
#> $X_S
#>       [,1]
#>  [1,]    2
#>  [2,]    0
#>  [3,]    1
#>  [4,]    1
#>  [5,]    1
#>  [6,]    2
#>  [7,]    0
#>  [8,]    2
#>  [9,]    2
#> [10,]    2
#> 
sample_data_Hp(n = 10, w = 0.1, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $X_D
#>       [,1] [,2]
#>  [1,]    2    2
#>  [2,]    0    1
#>  [3,]    1    2
#>  [4,]    2    0
#>  [5,]    0    2
#>  [6,]    1    1
#>  [7,]    1    1
#>  [8,]    2    0
#>  [9,]    0    0
#> [10,]    1    0
#> 
#> $X_S
#>       [,1] [,2]
#>  [1,]    2    0
#>  [2,]    0    1
#>  [3,]    2    1
#>  [4,]    2    0
#>  [5,]    0    2
#>  [6,]    1    0
#>  [7,]    1    1
#>  [8,]    2    0
#>  [9,]    0    0
#> [10,]    0    0
#> 
  
cases <- sample_data_Hp(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
tab <- table(cases$X_D, cases$X_S)
tab
#>    
#>       0   1   2
#>   0 112  99  44
#>   1 111 192 128
#>   2  35 154 125
estimate_w(tab)
#> [1] 0.2838007
```
