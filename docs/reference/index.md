# Package index

## All functions

- [`GT_convert_to_012()`](GT_convert_to_012.md) : Convert GT field from
  0/0, 0/1, 0\|1 etc. to 0, 1, 2

- [`add_errors_Ha_w()`](add_errors_Ha_w.md) : Add Ha-errors to table for
  one error probability, \$w\$

- [`add_errors_Ha_wTwR()`](add_errors_Ha_wTwR.md) : Add Ha-errors to
  table for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

- [`add_errors_Hp_w()`](add_errors_Hp_w.md) : Add Hp-errors to table for
  one error probability, \$w\$

- [`add_errors_Hp_wTwR()`](add_errors_Hp_wTwR.md) : Add Hp-errors to
  table for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

- [`add_errors_to_012genotypes()`](add_errors_to_012genotypes.md) : Add
  errors to 0/1/2 genotypes

- [`add_errors_to_genotypes()`](add_errors_to_genotypes.md) : Add errors
  to (phased) genotypes

- [`allele_probs_to_geno_probs()`](allele_probs_to_geno_probs.md) :
  Allele probabilities to genotype probabilities assuming Hardy-Weinberg
  equilibrium

- [`rbeta05()`](beta05.md) [`pbeta05()`](beta05.md)
  [`qbeta05()`](beta05.md) [`dbeta05()`](beta05.md) : Beta distribution
  on (0, 0.5)

- [`calc_LRs_w()`](calc_LRs_w.md) : Calculate LR for a profile for one
  error probability, \$w\$

- [`calc_LRs_wTwR()`](calc_LRs_wTwR.md) : Calculate LR for a profile for
  sample-specific error probabilities

- [`calc_WoE_wTwR_integrate_wT_mc()`](calc_WoE_wTwR_integrate_wT_mc.md)
  : Calculate WoE for a profile for sample-specific error probabilities
  integrated over the donor prior distribution using Monte Carlo
  integration

- [`calc_WoE_wTwR_integrate_wT_num()`](calc_WoE_wTwR_integrate_wT_num.md)
  : Calculate WoE for sample-specific error probabilities integrated
  over the donor prior distribution using numerical integration

- [`calc_WoE_wTwR_mleH2_wT_num()`](calc_WoE_wTwR_mleH2_wT_num.md) :

  Calculate WoE for sample-specific error probabilities using maximum
  likelihood of `wT` under \$H_2\$ using numerical optimisation

- [`calc_WoE_wTwR_profilemax_wT_num()`](calc_WoE_wTwR_profilemax_wT_num.md)
  : Calculate WoE for sample-specific error probabilities using profile
  likelihood using numerical optimisation

- [`calc_likelihood_numerator_Hp_wTwR()`](calc_likelihood.md)
  [`calc_likelihood_denominator_Ha_wTwR()`](calc_likelihood.md) :
  Calculate numerator (Hp) and denominator (Ha) likelihoods for a
  profile for sample-specific error probabilities

- [`d_LR_w`](d_LR_w.md) : Formulas for LR grouped by \$X^D\$ and \$X^S\$
  for one error probability, \$w\$

- [`d_LR_wTwR`](d_LR_wTwR.md) : Formulas for LR grouped by \$X^D\$ and
  \$X^S\$ for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

- [`d_formulas_Ha_w`](d_formulas_Ha_w.md) : Enumeration of possibilities
  under Hd for one error probability, \$w\$

- [`d_formulas_Ha_wTwR`](d_formulas_Ha_wTwR.md) : Enumeration of
  possibilities under Hd for sample-dependent error probabilities,
  \$w_t\$ and \$w_r\$

- [`d_formulas_Hp_w`](d_formulas_Hp_w.md) : Enumeration of possibilities
  under Hp for one error probability, \$w\$

- [`d_formulas_Hp_wTwR`](d_formulas_Hp_wTwR.md) : Enumeration of
  possibilities under Hp for sample-dependent error probabilities,
  \$w_t\$ and \$w_r\$

- [`d_formulas_single`](d_formulas_single.md) : Enumeration of
  possibilities for single observation

- [`d_prob_Ha_w`](d_prob_Ha_w.md) : Formulas under Hd grouped by \$X^D\$
  and \$X^S\$ for one error probability, \$w\$

- [`d_prob_Ha_wTwR`](d_prob_Ha_wTwR.md) : Formulas under Hd grouped by
  \$X^D\$ and \$X^S\$ for sample-dependent error probabilities, \$w_t\$
  and \$w_r\$

- [`d_prob_Hp_w`](d_prob_Hp_w.md) : Formulas under Hp grouped by \$X^D\$
  and \$X^S\$ for one error probability, \$w\$

- [`d_prob_Hp_wTwR`](d_prob_Hp_wTwR.md) : Formulas under Hp grouped by
  \$X^D\$ and \$X^S\$ for sample-dependent error probabilities, \$w_t\$
  and \$w_r\$

- [`d_probtable_Ha_w`](d_probtable_Ha_w.md) : Probabilties for efficient
  sampling under Hd for one error probability, \$w\$

- [`d_probtable_Ha_wTwR`](d_probtable_Ha_wTwR.md) : Probabilties for
  efficient sampling under Hd for one error probability, \$w\$

- [`d_probtable_Hp_w`](d_probtable_Hp_w.md) : Probabilties for efficient
  sampling under Hp for one error probability, \$w\$

- [`d_probtable_Hp_wTwR`](d_probtable_Hp_wTwR.md) : Probabilties for
  efficient sampling under Hp for sample-dependent error probabilities,
  \$w_t\$ and \$w_r\$

- [`estimate_w()`](estimate_w.md) : Estimate error probability, w

- [`estimate_w_bayesian()`](estimate_w_bayesian.md) : Estimate error
  probability, w, using a Bayesian approach

- [`estimate_w_se()`](estimate_w_se.md) : Estimate standard error of
  error probability, w

- [`generate_profilesets()`](generate_profilesets.md) : Get profile sets
  from cM threshold

- [`get_beta_mu_sigma()`](get_beta_mu_sigma.md) : Parameterisation
  conversion for beta distribution

- [`get_beta_parameters()`](get_beta_parameters.md) : Parameterisation
  conversion for beta distribution

- [`get_ns()`](get_ns.md) : Get counts from table

- [`graph_Ha_w`](graph_Ha_w.md) : Graph for enumeration of possibilities
  under Hd for one error probability, \$w\$

- [`graph_Ha_wTwR`](graph_Ha_wTwR.md) : Graph for enumeration of
  possibilities under Hd for sample-dependent error probabilities,
  \$w_t\$ and \$w_r\$

- [`graph_Hp_w`](graph_Hp_w.md) : Graph for enumeration of possibilities
  under Hp for one error probability, \$w\$

- [`graph_Hp_wTwR`](graph_Hp_wTwR.md) : Graph for enumeration of
  possibilities under Hp for sample-dependent error probabilities,
  \$w_t\$ and \$w_r\$

- [`graph_single`](graph_single.md) : Graph for enumeration of
  possibilities for single observation

- [`logliks_single_sample()`](logliks_single_sample.md) : Log-likelihood
  of \$w\$ from a single sample

- [`posterior_samples()`](posterior_samples.md) : Extract posterior
  samples

- [`posterior_samples_df()`](posterior_samples_df.md) : Extract
  posterior samples as data.frame

- [`reuse_genotype_probs()`](reuse_genotype_probs.md) : Re-use genotype
  probabilities

- [`sample_data_Ha_w()`](sample_data_Ha_w.md) : Sample cases under Ha
  for one error probability, \$w\$

- [`sample_data_Ha_wTwR()`](sample_data_Ha_wTwR.md) : Sample cases under
  Ha for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

- [`sample_data_Hp_w()`](sample_data_Hp_w.md) : Sample cases under Hp
  for one error probability, \$w\$

- [`sample_data_Hp_wTwR()`](sample_data_Hp_wTwR.md) : Sample cases under
  Hp for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

- [`sample_profiles_without_error()`](sample_profiles_without_error.md)
  : Sample genotype without error

- [`to012()`](to012.md) : Convert to 0/1/2 notation

- [`wT_posterior_Ha_naive()`](wT_posterior_Ha_naive.md) :

  Sample from posterior distribution of `wT` conditional on Ha using a
  naive MCMC sampler

- [`wT_posterior_Ha_stan()`](wT_posterior_Ha_stan.md) :

  Sample from posterior distribution of `wT` conditional on Ha using
  Stan
