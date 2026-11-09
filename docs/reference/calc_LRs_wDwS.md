# Calculate LR for a profile for one error probability, \$w\$

Calculate LR for a profile for one error probability, \$w\$

## Usage

``` r
calc_LRs_wDwS(xs, xd, wD, wS, p)
```

## Arguments

- xs:

  profile from suspect (of 0, 1, 2)

- xd:

  profile from case (of 0, 1, 2)

- wD:

  error probability for donor sample

- wS:

  error probability for PoI sample

- p:

  list of genotype probabilities (same length as `xd`/`xs`, or vector of
  length 3 for reuse)

## Examples

``` r
calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 4
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 4

calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> [1]  4 10
calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
  c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#> [1]  4 10
  
calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#> [1] 4 0

calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-5, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.9999199992 0.0001199928
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.9999199992 0.0001199928

calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-3, wS = 1e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.99199202 0.01192834
calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.99199202 0.01192834

calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-2, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#> [1] 3.95919624 0.07618684
calc_LRs_w(c(0, 0), c(0, 1), w = 5e-3, p = c(0.25, 0.25, 0.5))
#> [1] 3.959802 0.058242
```
