# Log-likelihood of \$w\$ from a single sample

Note that if `p = c(1/4, 1/2, 1/4)`, the likelihood does not depend on
`w` (and is equal to `log(p)`). In other words, the parameter `w` is
weakly identifiable when allele frequencies are close to \$0.5\$. Under
HWE, the expected information for \$w\$ decreases as the MAF approaches
\$0.5\$ and vanishes at MAF = 0.5 due to symmetry of the genotype
distribution.

## Usage

``` r
logliks_single_sample(x, p, w)
```

## Arguments

- x:

  Number of alternative alleles (0/1/2)

- p:

  list of genotype probabilities or vector of length 3 for single locus

- w:

  single number, genotyping error probability

## Details

Rare alleles (low MAF) will generally contribute more information about
\$w\$ than common variants near MAF = 0.5, assuming comparable sample
sizes.
