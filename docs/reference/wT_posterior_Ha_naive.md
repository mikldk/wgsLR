# Sample from posterior distribution of `wT` conditional on Ha using a naive MCMC sampler

Sample from posterior distribution of `wT` conditional on Ha using a
naive MCMC sampler

## Usage

``` r
wT_posterior_Ha_naive(
  xT,
  xR,
  wR,
  p,
  prior_a = 1,
  prior_b = 1,
  chains = 4,
  chains_function = lapply,
  warmup = 1000,
  iterations = 10000,
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

- chains_function:

  Function to run the chains (fx `lapply` or
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html))

- warmup:

  Number of warm-up iterations

- iterations:

  Number of iterations (after warm-up)

- ...:

  Not used
