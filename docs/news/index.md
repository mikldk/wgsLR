# Changelog

## wgsLR 0.0.3

- Added
  [`calc_WoE_wTwR_mleH2_wT_num()`](../reference/calc_WoE_wTwR_mleH2_wT_num.md)
- For
  [`calc_WoE_wTwR_profilemax_wT_num()`](../reference/calc_WoE_wTwR_profilemax_wT_num.md)
  and
  [`calc_WoE_wTwR_mleH2_wT_num()`](../reference/calc_WoE_wTwR_mleH2_wT_num.md):
  - Added options to use different optimisers on top of
    [`stats::optimise()`](https://rdrr.io/r/stats/optimize.html) (a
    version with an emperical check of unimodality and
    [`DEoptim::DEoptim()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.html)).
- Added
  [`calc_likelihood_numerator_Hp_wTwR()`](../reference/calc_likelihood.md)
  and
  [`calc_likelihood_denominator_Ha_wTwR()`](../reference/calc_likelihood.md)
- Added
  [`add_errors_to_012genotypes()`](../reference/add_errors_to_012genotypes.md)
- Added `wT_posterior_H*` functions and `cmdstanr` as a Suggested
  package
- Added
  [`logliks_single_sample()`](../reference/logliks_single_sample.md)

## wgsLR 0.0.2

- Added a `NEWS.md` file to track changes to the package.
- Renaming:
  - `xT` for trace sample (instead of `xD`)
  - `xR` for reference sample (instead of `xS`)
- Include functionality for sample specific error rates:
  - `wT` for **T**race sample and `wR` for **R**eference sample
  - Convention: postfix `_w` for same error rate in both samples and
    postfix `_wTwR` to allow for different error rates in donor (trace)
    and suspect (reference) sample.
  - Added functions and objects related to sample specific error
    probabilities:
    - [`calc_LRs_wTwR()`](../reference/calc_LRs_wTwR.md) renamed to
      [`calc_LRs_w()`](../reference/calc_LRs_w.md)
    - [`sample_data_Hp_wTwR()`](../reference/sample_data_Hp_wTwR.md),
      `sample_data_Hd_wTwR()`
    - [`add_errors_Hp_wTwR()`](../reference/add_errors_Hp_wTwR.md),
      `add_errors_Hd_wTwR()`
    - `d_formulas_Hd_wTwR`, `d_formulas_Hp_wTwR`,
    - `d_prob_Hp_wTwR`, `d_prob_Hd_wTwR`,
    - `d_LR_wTwR`
    - [`calc_WoE_wTwR_integrate_wT_mc()`](../reference/calc_WoE_wTwR_integrate_wT_mc.md)
    - [`calc_WoE_wTwR_integrate_wT_num()`](../reference/calc_WoE_wTwR_integrate_wT_num.md)
    - [`calc_WoE_wTwR_profilemax_wT_num()`](../reference/calc_WoE_wTwR_profilemax_wT_num.md)
  - Renamed functions and objects to allow for sample-specific $`w`$
    (cf. below), i.e.:
    - `calc_LRs()` renamed to
      [`calc_LRs_w()`](../reference/calc_LRs_w.md)
    - `sample_data_Hp()` renamed to
      [`sample_data_Hp_w()`](../reference/sample_data_Hp_w.md)
    - `sample_data_Hd()` renamed to `sample_data_Hd_w()`
    - `add_errors_Hp()` renamed to
      [`add_errors_Hp_w()`](../reference/add_errors_Hp_w.md)
    - `add_errors_Hd()` renamed to `add_errors_Hd_w()`
    - `d_formulas_Hd` renamed to `d_formulas_Hd_w`
    - `d_formulas_Hp` renamed to `d_formulas_Hp_w`
    - `d_prob_Hp` renamed to `d_prob_Hp_w`
    - `d_prob_Hd` renamed to `d_prob_Hd_w`
    - `d_prob_LR` renamed to `d_LR_w`
