# Re-use genotype probabilities

Re-use genotype probabilities

## Usage

``` r
reuse_genotype_probs(p, n)
```

## Arguments

- p:

  Genotype probabilities

- n:

  Number of times to re-use

## Examples

``` r
p <- c(0.2, 0.7, 0.1)
to012(sample_profiles_without_error(n = 2, p = p))
#>      [,1]
#> [1,]    1
#> [2,]    1

p_10loci <- reuse_genotype_probs(p, 10)
to012(sample_profiles_without_error(n = 2, p = p_10loci))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    0    1    1    1    1    1    1    1    1     1
#> [2,]    1    0    1    1    0    1    1    0    0     1
```
