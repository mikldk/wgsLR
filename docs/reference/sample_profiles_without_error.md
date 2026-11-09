# Sample genotype without error

Sample genotype without error

## Usage

``` r
sample_profiles_without_error(n, p)
```

## Arguments

- n:

  number of samples

- p:

  list of genotype probabilities or vector of length 3 for single locus

## Value

list, element for each locus is a matrix with n rows and two columns

## Examples

``` r
Z <- sample_profiles_without_error(n = 2, p = c(0.25, 0.25, 0.5))
to012(Z)
#>      [,1]
#> [1,]    1
#> [2,]    2
Z <- sample_profiles_without_error(n = 5, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
Z
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    0
#> [3,]    0    0
#> [4,]    0    0
#> [5,]    1    1
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    0
#> [3,]    1    1
#> [4,]    0    1
#> [5,]    1    0
#> 
to012(Z)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    0    0
#> [3,]    0    2
#> [4,]    0    1
#> [5,]    2    1
```
