# Allele probabilities to genotype probabilities assuming Hardy-Weinberg equilibrium

Allele probabilities to genotype probabilities assuming Hardy-Weinberg
equilibrium

## Usage

``` r
allele_probs_to_geno_probs(x)
```

## Arguments

- x:

  Allele probs for reference (0, so P(00) = q^2 etc.)

## Examples

``` r
allele_probs_to_geno_probs(c(0.1, 0.2))
#> [[1]]
#> [1] 0.01 0.18 0.81
#> 
#> [[2]]
#> [1] 0.04 0.32 0.64
#> 
```
