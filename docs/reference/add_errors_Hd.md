# Add Hp-errors to table

Add Hp-errors to table

## Usage

``` r
add_errors_Hd(tab, w)
```

## Arguments

- tab:

  table to add errors to

- w:

  error probability

## Examples

``` r
Z_D <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
Z_S <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z_D), to012(Z_S))
tab
#>    
#>       0   1   2
#>   0 141  57 191
#>   1  45  17  66
#>   2 167  58 258
new_tab <- add_errors_Hd(tab, w = 0.15)
new_tab
#>      [,1] [,2] [,3]
#> [1,]   87  111  124
#> [2,]   92   92  128
#> [3,]  112  104  150
estimate_w(new_tab) # wrong!
#> [1] 0.5
Z <- sample_profiles_without_error(n = 10000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z), to012(Z))
tab
#>    
#>        0    1    2
#>   0 3756    0    0
#>   1    0 1277    0
#>   2    0    0 4967
new_tab <- add_errors_Hd(tab, w = 0.25)
new_tab
#>      [,1] [,2] [,3]
#> [1,] 1255 1062  356
#> [2,] 1066 1671 1276
#> [3,]  343 1283 1688
estimate_w(new_tab) # ok
#> [1] 0.2492552
```
