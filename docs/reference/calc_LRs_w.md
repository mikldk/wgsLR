# Calculate LR for a profile for one error probability, \$w\$

Calculate LR for a profile for one error probability, \$w\$

## Usage

``` r
calc_LRs_w(xT, xR, w, p)
```

## Arguments

- xT:

  profile from case (of 0, 1, 2)

- xR:

  profile from suspect (of 0, 1, 2)

- w:

  error probability

- p:

  list of genotype probabilities (same length as `xT`/`xR`, or vector of
  length 3 for reuse)

## Examples

``` r
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 4
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> [1]  4 10
  
calc_LRs_w(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 0
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.9999199992 0.0001199928
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.99199202 0.01192834
```
