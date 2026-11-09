# Add Hd-errors to table for sample-dependent error probabilities, \$w_D\$ and \$w_S\$

Add Hd-errors to table for sample-dependent error probabilities, \$w_D\$
and \$w_S\$

## Usage

``` r
add_errors_Hd_wDwS(tab, wD, wS)
```

## Arguments

- tab:

  table to add errors to

- wD:

  error probability for donor sample

- wS:

  error probability for PoI sample

## Examples

``` r
Z_D <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
Z_S <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z_D), to012(Z_S))
tab
#>    
#>       0   1   2
#>   0  80  55 128
#>   1  55  70 122
#>   2 132 118 240
add_errors_Hd_wDwS(tab, wD = 0.1, wS = 1e-6)
#>      [,1] [,2] [,3]
#> [1,]   73   55  112
#> [2,]   84   85  160
#> [3,]  110  103  218
add_errors_Hd_w(tab, w = 0.05)
#>      [,1] [,2] [,3]
#> [1,]   70   74  109
#> [2,]   58   95  130
#> [3,]  112  139  213
```
