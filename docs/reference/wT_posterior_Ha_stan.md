# Sample from posterior distribution of `wT` conditional on Ha using Stan

Sample from posterior distribution of `wT` conditional on Ha using Stan

## Usage

``` r
wT_posterior_Ha_stan(
  xT,
  xR,
  wR,
  p,
  prior_a = 1,
  prior_b = 1,
  chains = 4,
  show_messages = FALSE,
  ...
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

- prior_a:

  First shape parameter for prior beta distribution

- prior_b:

  Second shape parameter for prior beta distribution

- chains:

  Number of chains to run

- ...:

  Passed to
  [`cmdstanr::sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
