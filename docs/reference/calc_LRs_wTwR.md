# Calculate LR for a profile for sample-specific error probabilities

Calculate LR for a profile for sample-specific error probabilities

## Usage

``` r
calc_LRs_wTwR(xT, xR, wT, wR, p)
```

## Arguments

- xT:

  profile from case (of 0, 1, 2)

- xR:

  profile from suspect (of 0, 1, 2)

- wT:

  error probability for donor sample

- wR:

  error probability for PoI sample

- p:

  list of genotype probabilities (same length as `xT`/`xR`, or vector of
  length 3 for reuse)

## Examples

``` r
calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 4
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 4

calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> [1]  4 10
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> [1]  4 10
  
calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 0

calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-5, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.9999199992 0.0001199928
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.9999199992 0.0001199928

calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-3, wR = 1e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.99199202 0.01192834
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.99199202 0.01192834

calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-6, p = c(0.25, 0.25, 0.5))
#> [1] 3.9591962 0.0399996
calc_LRs_w(c(0, 0), c(0, 1), w = 5e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.959802 0.058242
```
