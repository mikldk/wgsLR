
# `wgsLR`: Shotgun sequencing for human identification: Dynamic SNP marker sets and likelihood ratio calculations accounting for errors

Please refer to the online documentation at
<https://mikldk.github.io/wgsLR>, including the vignettes.

## A few small examples

### Estimating the genotype error probability, $w$

``` r
cases <- wgsLR::sample_data_Hp(n = 1000, w = 0.1, p = c(0.25, 0.25, 0.5))
tab <- table(cases$X_D, cases$X_S)
tab
```

    ##    
    ##       0   1   2
    ##   0 249  60   7
    ##   1  67 122  87
    ##   2   7  81 320

``` r
w_mle <- wgsLR::estimate_w(tab)
w_mle
```

    ## [1] 0.09822341

### Calculating likelihood ratios ($LR$’s)

Errors possible and non-matching genotypes:

``` r
LR_contribs <- wgsLR::calc_LRs(xs = c(0, 0, 2, 2), 
                               xd = c(1, 0, 2, 2), 
                               w = 0.001, 
                               p = c(0.25, 0.25, 0.5))
LR_contribs
```

    ## [1] 0.01192834 3.99199202 1.99799850 1.99799850

``` r
prod(LR_contribs)
```

    ## [1] 0.1900904
