# Add Hd-errors to table for one error probability, \$w\$

Add Hd-errors to table for one error probability, \$w\$

## Usage

``` r
add_errors_Hd_w(tab, w)
```

## Arguments

- tab:

  table to add errors to

- w:

  error probability

## Examples

``` r
zT <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
zR <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(zT), to012(zR))
tab
#>    
#>       0   1   2
#>   0  64  62 135
#>   1  63  71 122
#>   2 109 116 258
new_tab <- add_errors_Hd_w(tab, w = 0.15)
#> Error in split.default(x = seq_len(nrow(x)), f = f, drop = drop, ...): group length is 0 but data length > 0
new_tab
#> Error: object 'new_tab' not found
estimate_w(new_tab) # wrong!
#> Error: object 'new_tab' not found
Z <- sample_profiles_without_error(n = 10000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z), to012(Z))
tab
#>    
#>        0    1    2
#>   0 2523    0    0
#>   1    0 2513    0
#>   2    0    0 4964
new_tab <- add_errors_Hp_w(tab, w = 0.25)
#> Error in add_errors_Hp_w(tab, w = 0.25): isTRUE(all.equal(sum(tab), sum(new_tab))) is not TRUE
new_tab
#> Error: object 'new_tab' not found
estimate_w(new_tab) # ok
#> Error: object 'new_tab' not found
```
