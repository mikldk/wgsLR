# Calculate WoE for sample-specific error probabilities integrated over the donor prior distribution using exact integration

Note that the WoE for each marker is calculated by integrating over the
prior of \$w_t\$ separately under \$H_1\$ and \$H_2\$. An alternative is
implemented in
[`calc_WoE_wTwR_integrate_wT_mc()`](calc_WoE_wTwR_integrate_wT_mc.md) as
the expected value of log10 of \$LR\$ under a prior of \$w_t\$.

## Usage

``` r
calc_WoE_wTwR_integrate_wT_exact_markerwise(
  xT,
  xR,
  shape1T_H1,
  shape2T_H1,
  shape1T_H2,
  shape2T_H2,
  wR,
  p,
  use_mpfr = TRUE,
  stop_on_infinite = TRUE,
  mpfr_precision = 256
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

- use_mpfr:

  use higher precision numbers via the `Rmpfr` package

- stop_on_infinite:

  stop if infinite numbers are encountered (if so, try
  `use_mpfr = TRUE`)

- mpfr_precision:

  number of bits to use

## Details

Results from this can be compared to those from
[`calc_WoE_wTwR_integrate_wT_mc_markerwise()`](calc_WoE_wTwR_integrate_wT_mc_markerwise.md).

## Examples

``` r
calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5)) |> log10()
#> [1]  0.5976032 -1.3971946

shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-5, a = 0, b = 0.5)
# curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
calc_WoE_wTwR_integrate_wT_mc_markerwise(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_H1 = shpT[1], shape2T_H1 = shpT[2],
  shape1T_H2 = shpT[1], shape2T_H2 = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 1000)
#> [1]  0.5976104 -1.4193728
  
calc_WoE_wTwR_integrate_wT_exact_markerwise(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_H1 = shpT[1], shape2T_H1 = shpT[2],
  shape1T_H2 = shpT[1], shape2T_H2 = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5))
#> [1]  0.5975988 -1.3976413
```
