# Calculate WoE for sample-specific error probabilities using profile likelihood using numerical optimisation

Calculate WoE for sample-specific error probabilities using profile
likelihood using numerical optimisation

## Usage

``` r
calc_WoE_wTwR_profilemax_wT_num(
  xT,
  xR,
  wR,
  p,
  opttype = c("optimise_strict", "optimise_nocheck", "DEoptim")
)
```

## Arguments

- xT:

  profile from case (of 0, 1, 2)

- xR:

  profile from suspect (of 0, 1, 2)

- wR:

  error probability for PoI sample

- p:

  list of genotype probabilities (same length as `xT`/`xR`, or vector of
  length 3 for reuse)

- opttype:

  Optimiser type `"optimise_strict"`, `"optimise_nocheck"`, or
  `"DEoptim"`

## Examples

``` r
calc_LRs_wTwR(
  xT = c(0, 0), 
  xR = c(0, 1), 
  wT = 1e-2, 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5)) |> log10() |> sum()
#> [1] -0.7995914

calc_WoE_wTwR_profilemax_wT_num(
  xT = c(0, 0), 
  xR = c(0, 1), 
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5))
#> $wT_Hp
#> [1] 0.2499892
#> 
#> $wT_Ha
#> [1] 0.5
#> 
#> $log10PrE_Hp
#> [1] -2.18098
#> 
#> $log10PrE_Ha
#> [1] -2.408227
#> 
#> $WoE
#> [1] 0.2272467
#> 

shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-7, a = 0, b = 0.5)
# curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
calc_WoE_wTwR_integrate_wT_mc(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_Hp = shpT[1], 
  shape2T_Hp = shpT[2],
  shape1T_Ha = shpT[1], 
  shape2T_Ha = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5),
  n_samples = 1000)
#> [1] -0.7989437
  
calc_WoE_wTwR_integrate_wT_num(
  xT = c(0, 0), 
  xR = c(0, 1), 
  shape1T_Hp = shpT[1], 
  shape2T_Hp = shpT[2],
  shape1T_Ha = shpT[1], 
  shape2T_Ha = shpT[2],
  wR = 1e-5, 
  p = c(0.25, 0.25, 0.5))
#> [1] -0.7996046
  
```
