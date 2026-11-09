# Add errors to 0/1/2 genotypes

Add errors to 0/1/2 genotypes

## Usage

``` r
add_errors_to_012genotypes(z, w)
```

## Arguments

- w:

  error probability

- Z:

  genotypes in 0/1/2 format, e.g. created by
  [`sample_profiles_without_error()`](sample_profiles_without_error.md)
  followed by [`to012()`](to012.md)

## Value

list, element for each locus is a matrix with n rows and two columns

## Examples

``` r
Z <- sample_profiles_without_error(n = 100, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
X <- add_errors_to_genotypes(Z, 0.5)
table(to012(X))
#> 
#>   0   1   2 
#>  55 102  43 
X <- add_errors_to_012genotypes(to012(Z), 0.5)
table(X)
#> X
#>   0   1   2 
#>  45 112  43 
estimate_w(tab)
#> Error: object 'tab' not found

Z012 <- to012(Z)
X1 <- add_errors_to_012genotypes(Z012, 0.1)
X2 <- add_errors_to_012genotypes(Z012, 0.1)
tab <- table(X1, X2)
tab
#>    X2
#> X1   0  1  2
#>   0 26 17  3
#>   1 14 71 13
#>   2  0 18 38
estimate_w(tab)
#> [1] 0.1047212
```
