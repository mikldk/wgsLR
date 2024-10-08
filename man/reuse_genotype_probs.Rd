% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{reuse_genotype_probs}
\alias{reuse_genotype_probs}
\title{Re-use genotype probabilities}
\usage{
reuse_genotype_probs(p, n)
}
\description{
p <- c(0.2, 0.7, 0.1)
to012(sample_profiles_without_error(n = 2, p = p))
}
\details{
p_10loci <- reuse_genotype_probs(p, 10)
to012(sample_profiles_without_error(n = 2, p = p_10loci))
}
