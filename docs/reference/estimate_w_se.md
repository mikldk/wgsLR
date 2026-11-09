# Estimate standard error of error probability, w

Note that this relies on the Hessian approximation returned by
[`numDeriv::hessian()`](https://rdrr.io/pkg/numDeriv/man/hessian.html).

## Usage

``` r
estimate_w_se(x, w, method.args = list())
```

## Arguments

- x:

  Table, see [`estimate_w()`](estimate_w.md)

- w:

  estimated error probability w

- method.args:

  see [`numDeriv::grad()`](https://rdrr.io/pkg/numDeriv/man/grad.html)

## Examples

``` r
x <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
x
#>      [,1] [,2] [,3]
#> [1,] 1000   12    1
#> [2,]   10  100    7
#> [3,]    2    8  200
w <- estimate_w(x)
w
#> [1] 0.00815444
estimate_w_se(x, w)
#> [1] 0.001248947
```
