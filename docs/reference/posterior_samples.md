# Extract posterior samples

Extract posterior samples

## Usage

``` r
posterior_samples(x)
```

## Arguments

- x:

  Result from [`estimate_w_bayesian()`](estimate_w_bayesian.md)

## Examples

``` r
tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
tab1
#>      [,1] [,2] [,3]
#> [1,] 1000   12    1
#> [2,]   10  100    7
#> [3,]    2    8  200
y <- estimate_w_bayesian(tab1)
q <- posterior_samples(y)
mean(q); #plot(q, type = "l"); hist(q)
#> [1] 0.008422653
```
