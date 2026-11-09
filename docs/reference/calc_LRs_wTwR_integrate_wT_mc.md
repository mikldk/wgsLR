# Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using Monte Carlo integration

Calculate LR for a profile for sample-specific error probabilities
integrated over the donor prior distribution using Monte Carlo
integration

## Usage

``` r
calc_LRs_wTwR_integrate_wT_mc(
  xT,
  xR,
  shape1T_H1,
  shape2T_H1,
  shape1T_H2,
  shape2T_H2,
  wR,
  p,
  n_samples = 1000
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

- n_samples:

  number of random samples from each prior distribution

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
# curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
calc_LRs_wTwR_integrate_wT_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1D = 1, shape2D = 1, 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 100)
#> Error in calc_LRs_wTwR_integrate_wT_mc(xT = c(0, 0), xR = c(0, 1), shape1D = 1,     shape2D = 1, wR = 1e-05, p = c(0.25, 0.25, 0.5), n_samples = 100): unused arguments (shape1D = 1, shape2D = 1)
# curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
  
shpS <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-7, a = 0, b = 0.5)
integrate(function(x) dbeta05(x, shape1 = shpS[1L], shape2 = shpS[2L]), lower = 1e-12, upper = 0.5, rel.tol = 1e-6)
#> 1 with absolute error < 6e-07
calc_LRs_wTwR_integrate_wTwR_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1D = shpD[1], shape2D = shpD[2], 
  shape1S = shpS[1], shape2S = shpS[2], 
  p = c(0.25, 0.25, 0.5),
  n_samples = 100)
#> Error in calc_LRs_wTwR_integrate_wTwR_mc(xT = c(0, 0), xR = c(0, 1), shape1D = shpD[1],     shape2D = shpD[2], shape1S = shpS[1], shape2S = shpS[2],     p = c(0.25, 0.25, 0.5), n_samples = 100): could not find function "calc_LRs_wTwR_integrate_wTwR_mc"
# curve(dbeta05(x, shpS[1], shpS[2]), from = 0, to = 0.5)
```
