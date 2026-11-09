# Parameterisation conversion for beta distribution

Get beta distribution's two shape parameters from mean value and
variance.

## Usage

``` r
get_beta_parameters(mu, sigmasq, a = 0, b = 0.5)
```

## Arguments

- mu:

  mean value

- sigmasq:

  variance

- a:

  lower support bound

- b:

  upper support bound

## Examples

``` r
a <- 2
b <- 6
m <- a / (a + b)
v <- (a*b)/((a+b)^2 * (a + b +1))
get_beta_parameters(m, v)
#> [1] 1 1
get_beta_parameters(1e-2, 1e-3)
#> [1] 0.078 3.822
# p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
# p <- get_beta_parameters(0.1, 0.001); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
# p <- get_beta_parameters(0.1, 0.0001); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)

s12 <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-3, a = 0, b = 0.5)
x <- rbeta05(1000, s12[1L], s12[2L])
mean(x); var(x)
#> [1] 0.01023009
#> [1] 0.0009999333

s12 <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-7, a = 0, b = 0.5)
x <- rbeta05(10000, s12[1L], s12[2L])
mean(x); var(x)
#> [1] 6.569014e-06
#> [1] 3.548886e-08
```
