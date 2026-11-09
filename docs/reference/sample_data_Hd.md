# Sample cases under Hd

One latent genotype, ZD, for true donor (D) and one latent genotype, ZS,
for suspect (S).

## Usage

``` r
sample_data_Hd(n, w, p)
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
sample_data_Hd(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#> $X_D
#>       [,1]
#>  [1,]    2
#>  [2,]    2
#>  [3,]    1
#>  [4,]    0
#>  [5,]    1
#>  [6,]    0
#>  [7,]    0
#>  [8,]    0
#>  [9,]    2
#> [10,]    1
#> 
#> $X_S
#>       [,1]
#>  [1,]    2
#>  [2,]    0
#>  [3,]    2
#>  [4,]    2
#>  [5,]    0
#>  [6,]    2
#>  [7,]    1
#>  [8,]    0
#>  [9,]    0
#> [10,]    1
#> 
sample_data_Hd(n = 10, w = 0.1, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> $X_D
#>       [,1] [,2]
#>  [1,]    1    1
#>  [2,]    1    0
#>  [3,]    0    1
#>  [4,]    1    1
#>  [5,]    0    0
#>  [6,]    0    1
#>  [7,]    1    2
#>  [8,]    2    1
#>  [9,]    2    0
#> [10,]    1    0
#> 
#> $X_S
#>       [,1] [,2]
#>  [1,]    1    0
#>  [2,]    2    1
#>  [3,]    1    0
#>  [4,]    2    2
#>  [5,]    0    1
#>  [6,]    1    1
#>  [7,]    0    0
#>  [8,]    2    1
#>  [9,]    2    1
#> [10,]    1    0
#> 
  
cases <- sample_data_Hd(n = 1000, w = 0, p = c(0.25, 0.25, 0.5))
tab <- table(cases$X_D, cases$X_S)
tab
#>    
#>       0   1   2
#>   0 162  49 191
#>   1  43  16  60
#>   2 196  57 226
```
