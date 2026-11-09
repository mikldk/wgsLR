# Calculate WoE for sample-specific error probabilities integrated over the donor prior distribution using numerical integration

Note that the numerical integration can have problems for prior
distributions with low variance. Remember to verify results, e.g. by
comparing to Monte Carlo integration by
[`calc_WoE_wTwR_integrate_wT_mc()`](calc_WoE_wTwR_integrate_wT_mc.md).

## Usage

``` r
calc_WoE_wTwR_integrate_wT_num(
  xT,
  xR,
  shape1T_Hp,
  shape2T_Hp,
  shape1T_Ha,
  shape2T_Ha,
  wR,
  p
)
```

## Arguments

- xT:

  profile from case (of 0, 1, 2)

- xR:

  profile from suspect (of 0, 1, 2)

- shape1T_Hp:

  under \$H_p\$ `wT` has beta prior on (0, 0.5) with parameters
  `shape1T_Hp` and `shape2T_Hp`

- shape2T_Hp:

  see `shape1T_Hp`

- shape1T_Ha:

  under \$H_a\$ `wT` has beta prior on (0, 0.5) with parameters
  `shape1T_Ha` and `shape2T_Ha`

- shape2T_Ha:

  see `shape1T_Ha`

- wR:

  error probability for PoI sample

- p:

  list of genotype probabilities (same length as `xT`/`xR`, or vector of
  length 3 for reuse)

## Examples

``` r
calc_LRs_wTwR(
  xT = c(0, 0), 
  xR = c(0, 1), 
  wT = 1e-2, 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5)) |> log10() |> sum()
#> [1] -0.7995914

shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-7, a = 0, b = 0.5)
# curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
z1 <- calc_WoE_wTwR_integrate_wT_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_Hp = shpT[1], 
  shape2T_Hp = shpT[2],
  shape1T_Ha = shpT[1], 
  shape2T_Ha = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 1000)
z1
#> [1] -0.8009055

z2 <- calc_WoE_wTwR_integrate_wT_num(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_Hp = shpT[1], 
  shape2T_Hp = shpT[2],
  shape1T_Ha = shpT[1], 
  shape2T_Ha = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5))
z2
#> [1] -0.7996046
  
```
