# Add Ha-errors to table for sample-dependent error probabilities, \$w_t\$ and \$w_r\$

Add Ha-errors to table for sample-dependent error probabilities, \$w_t\$
and \$w_r\$

## Usage

``` r
add_errors_Ha_wTwR(tab, wT, wR)
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
tab <- table(to012(zT), to012(zR))
tab
#>    
#>       0   1   2
#>   0  80  55 128
#>   1  55  70 122
#>   2 132 118 240
add_errors_Ha_wTwR(tab, wT = 0.1, wR = 1e-6)
#>      [,1] [,2] [,3]
#> [1,]   73   55  112
#> [2,]   84   85  160
#> [3,]  110  103  218
add_errors_Ha_w(tab, w = 0.05)
#>      [,1] [,2] [,3]
#> [1,]   70   74  109
#> [2,]   58   95  130
#> [3,]  112  139  213
```
