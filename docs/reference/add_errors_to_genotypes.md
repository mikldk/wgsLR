# Add errors to (phased) genotypes

Add errors to (phased) genotypes

## Usage

``` r
add_errors_to_genotypes(Z, w, overdisp_var = NULL)
```

## Arguments

- Z:

  genotypes, e.g. created by
  [`sample_profiles_without_error()`](sample_profiles_without_error.md)

- w:

  error probability

- overdisp_var:

  if set, use as modelling overdispersion in \$w\$ such that \$w\$
  follows a Beta distribution with mean value \$w\$ and variance
  `overdisp_var`

## Value

list, element for each locus is a matrix with n rows and two columns

## Examples

``` r
Z <- sample_profiles_without_error(n = 5, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
to012(Z)
#>      [,1] [,2]
#> [1,]    2    1
#> [2,]    1    1
#> [3,]    2    2
#> [4,]    2    1
#> [5,]    2    1
X <- add_errors_to_genotypes(Z, 0.5)
X
#> [[1]]
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1    0
#> [3,]    1    1
#> [4,]    0    1
#> [5,]    1    0
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    1    0
#> [3,]    0    1
#> [4,]    1    0
#> [5,]    0    0
#> 
to012(Z) - to012(add_errors_to_genotypes(Z, 0.0))
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> [3,]    0    0
#> [4,]    0    0
#> [5,]    0    0


Z <- sample_profiles_without_error(n = 1000, p = c(0.5, 0.4, 0.1))
X1 <- add_errors_to_genotypes(Z, 0.1)
X2 <- add_errors_to_genotypes(Z, 0.1)
tab <- table(to012(X1), to012(X2))
tab
#>    
#>       0   1   2
#>   0 339 100   8
#>   1  95 294  44
#>   2   8  51  61
estimate_w(tab)
#> [1] 0.09788312

X1 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
X2 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
tab <- table(to012(X1), to012(X2))
tab
#>    
#>       0   1   2
#>   0 317 101  16
#>   1 101 292  42
#>   2  16  38  77
estimate_w(tab)
#> [1] 0.1074347
# p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)


# p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
X1 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
X2 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
tab <- table(to012(X1), to012(X2))
tab
#>    
#>       0   1   2
#>   0 332  90  11
#>   1 104 302  44
#>   2  12  37  68
estimate_w(tab)
#> [1] 0.09763069
```
