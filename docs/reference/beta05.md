# Beta distribution on (0, 0.5)

Beta distribution on (0, 0.5)

## Usage

``` r
rbeta05(n, shape1, shape2)

pbeta05(x, shape1, shape2)

qbeta05(x, shape1, shape2)

dbeta05(x, shape1, shape2, log = FALSE)
```

## Arguments

- n:

  number of random samples to draw

- shape1:

  first shape parameter

- shape2:

  second shape parameter

- x:

  number between 0 and 0.5

- log:

  return result on log scale

## Examples

``` r
dbeta05(0.2, 1, 5)
#> [1] 1.296
dbeta05(0.2, 1, 5, log = TRUE)
#> [1] 0.2592826
dbeta05(0.2, 1, 5) |> log()
#> [1] 0.2592826

pbeta05(0.5, 1, 5)
#> [1] 1

qbeta05(1, 1, 5)
#> [1] 0.5

#rbeta05(100, 1, 5) |> hist(probability = TRUE)
#curve(dbeta05(x, 1, 5), from = 0, to = 0.5, add = TRUE)
```
