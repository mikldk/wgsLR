# Add Hd-errors to table for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

Add Hd-errors to table for sample-dependent error probabilities, \$w_t\$
and \$w_r\$

## Usage

``` r
add_errors_Hd_wTwR(tab, wT, wR)
```

## Arguments

- tab:

  table to add errors to

- wT:

  error probability for donor sample

- wR:

  error probability for PoI sample

## Examples

``` r
zT <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
zR <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z_T), to012(zR))
#> Error: object 'Z_T' not found
tab
#> Error: object 'tab' not found
add_errors_Hd_wTwR(tab, wT = 0.1, wR = 1e-6)
#> Error: object 'tab' not found
add_errors_Hd_w(tab, w = 0.05)
#> Error: object 'tab' not found
```
