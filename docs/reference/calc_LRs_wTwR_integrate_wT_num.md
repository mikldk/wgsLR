# Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using numerical integration

NOTE: Unless you have a reason, please use
[`calc_LRs_wTwR_integrate_wT()`](calc_LRs_wTwR_integrate_wT.md) instead.

## Usage

``` r
calc_LRs_wTwR_integrate_wT_num(
  xT,
  xR,
  shape1T_H1,
  shape2T_H1,
  shape1T_H2,
  shape2T_H2,
  wR,
  p,
  lower_int = 0,
  ...
)
```

## Arguments

- xT:

  profile from case (of 0, 1, 2)

- xR:

  profile from suspect (of 0, 1, 2)

- shape1T_H1:

  Under \$H_1\$ (in \$LR\$'s numerator), `wT` has beta prior on (0, 0.5)
  with parameters `shape1T_H1` and `shape2T_H1`

- shape2T_H1:

  see `shape1T_Hp`

- shape1T_H2:

  Under \$H_2\$ (in \$LR\$'s denominator), `wT` has beta prior (on
  0-0.5) with parameters `shape1T_H2` and `shape2T_H2`

- shape2T_H2:

  see `shape1T_H2`

- wR:

  error probability for PoI sample

- p:

  list of genotype probabilities (same length as `xT`/`xR`, or vector of
  length 3 for reuse)

- lower_int:

  lowest value to integrate from; should be 0, but numerical instability
  can make this problematic

- ...:

  pass on to [`integrate()`](https://rdrr.io/r/stats/integrate.html),
  e.g., `rel.tol` and `abs.tol`

## Examples

``` r
calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#> [1] 3.95916097 0.04006872

shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
calc_LRs_wTwR_integrate_wT_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1D = shpD[1], shape2D = shpD[2], 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 100)
#> Error in calc_LRs_wTwR_integrate_wT_mc(xT = c(0, 0), xR = c(0, 1), shape1D = shpD[1],     shape2D = shpD[2], wR = 1e-05, p = c(0.25, 0.25, 0.5), n_samples = 100): unused arguments (shape1D = shpD[1], shape2D = shpD[2])
calc_LRs_wTwR_integrate_wT_num(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1D = shpD[1], shape2D = shpD[2], 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5))
#> Error in f(x, ...): unused arguments (shape1D = 0.029, shape2D = 1.421)
# curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)

calc_LRs_wTwR_integrate_wT_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1D = 1, shape2D = 1, 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 100)
#> Error in calc_LRs_wTwR_integrate_wT_mc(xT = c(0, 0), xR = c(0, 1), shape1D = 1,     shape2D = 1, wR = 1e-05, p = c(0.25, 0.25, 0.5), n_samples = 100): unused arguments (shape1D = 1, shape2D = 1)
calc_LRs_wTwR_integrate_wT_num(
  xT = c(0, 0),
  xR = c(0, 1),
  shape1D = 1, shape2D = 1,
  wR = 1e-5,
  p = c(0.25, 0.25, 0.5),
  n_samples = 100)
#> Error in f(x, ...): unused arguments (shape1D = 1, shape2D = 1, n_samples = 100)
# curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
  
```
