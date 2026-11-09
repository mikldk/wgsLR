# Calculate numerator (Hp) and denominator (Ha) likelihoods for a profile for sample-specific error probabilities

Calculate numerator (Hp) and denominator (Ha) likelihoods for a profile
for sample-specific error probabilities

## Usage

``` r
calc_likelihood_numerator_Hp_wTwR(xT, xR, wT, wR, p)

calc_likelihood_denominator_Ha_wTwR(xT, xR, wT, wR, p)
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
nums <- calc_likelihood_numerator_Hp_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-6, p = c(0.25, 0.25, 0.5))
dens <- calc_likelihood_denominator_Ha_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-6, p = c(0.25, 0.25, 0.5))
LRs <- calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-6, p = c(0.25, 0.25, 0.5))
isTRUE(all.equal(LRs, nums/dens))
#> [1] TRUE

p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 100)
wT <- 1e-2
wR <- 1e-6
Hp_cases <- sample_data_Hp_wTwR(n = 1, wT = wT, wR = wR, p = p)
xT <- Hp_cases$xT[1, ]
xR <- Hp_cases$xR[1, ]
num <- calc_likelihood_numerator_Hp_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p) |> prod()
den <- calc_likelihood_denominator_Ha_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p) |> prod()
LR <- calc_LRs_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p) |> prod()
isTRUE(all.equal(LR, num/den))
#> [1] TRUE
```
