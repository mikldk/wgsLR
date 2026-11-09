# Extract posterior samples as data.frame

Extract posterior samples as data.frame

## Usage

``` r
posterior_samples_df(x)
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
q <- posterior_samples_df(y)
head(q)
#>   chain iteration      sample
#> 1     1         1 0.008936453
#> 2     1         2 0.008936453
#> 3     1         3 0.008936453
#> 4     1         4 0.008936453
#> 5     1         5 0.008936453
#> 6     1         6 0.008936453
mean(q$sample); #ggplot(q, aes(iteration, sample, colour = factor(chain))) + geom_line()
#> [1] 0.008427986
```
