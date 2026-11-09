# Estimate error probability, w

Note that `tol = .Machine$double.eps` unless overwritten.

## Usage

``` r
estimate_w(x, use_mpfr = FALSE, ...)
```

## Arguments

- x:

  either a 3x3 contingency table or 3-vector with counts (`n1`, `n2`,
  `n3`) where `n1 = sum(diag(x))`, `n2 = x[1L, 3L] + x[3L, 1L]`,
  `n3 = sum(x) - n1 - n2`

- use_mpfr:

  whether to use
  [`Rmpfr::optimizeR()`](https://rdrr.io/pkg/Rmpfr/man/optimizeR.html)\]
  for arbitrary precision

- ...:

  passed on to [`optimise()`](https://rdrr.io/r/stats/optimize.html) or
  to [`Rmpfr::optimizeR`](https://rdrr.io/pkg/Rmpfr/man/optimizeR.html)
  if `use_mpfr = TRUE`

## Examples

``` r
tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
tab1
#>      [,1] [,2] [,3]
#> [1,] 1000   12    1
#> [2,]   10  100    7
#> [3,]    2    8  200
estimate_w(tab1)
#> [1] 0.00815444
n1 <- sum(diag(tab1))
n2 <- tab1[1L, 3L] + tab1[3L, 1L]
n3 <- sum(tab1) - n1 - n2
estimate_w(c(n1, n2, n3))
#> [1] 0.00815444

tab2 <- structure(c(40000L, 30L, 5L, 15L, 400L, 2L, 5L, 5L, 350L),
        dim = c(3L, 3L), class = "table")
tab2
#>       [,1]  [,2]  [,3]
#> [1,] 40000    15     5
#> [2,]    30   400     5
#> [3,]     5     2   350
estimate_w(tab2)
#> [1] 0.0004414364
estimate_w(tab2, use_mpfr = TRUE)
#> 1 'mpfr' number of precision  132   bits 
#> [1] 0.00044143635878284077344503195817562240248647
```
