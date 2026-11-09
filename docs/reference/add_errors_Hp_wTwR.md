# Add Hp-errors to table for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

Add Hp-errors to table for sample-dependent error probabilities, \$w_t\$
and \$w_r\$

## Usage

``` r
add_errors_Hp_wTwR(tab, wT, wR)
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
Z <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z))
tab
#> 
#>   0   1   2 
#> 254 230 516 
add_errors_Hp_wTwR(tab, wT = 0.1, wR = 1e-6)
#>      [,1] [,2] [,3]
#> [1,]  201   15    4
#> [2,]   52  191   96
#> [3,]    1   24  416
add_errors_Hp_w(tab, w = 0.05)
#>      [,1] [,2] [,3]
#> [1,]  201   39    2
#> [2,]   37  189   47
#> [3,]    3   63  419
```
