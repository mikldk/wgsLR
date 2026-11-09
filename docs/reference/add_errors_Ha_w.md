# Add Ha-errors to table for one error probability, \$w\$

Add Ha-errors to table for one error probability, \$w\$

## Usage

``` r
add_errors_Ha_w(tab, w)
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
new_tab <- add_errors_Ha_w(tab, w = 0.15)
new_tab
#>      [,1] [,2] [,3]
#> [1,]   45   93  101
#> [2,]   72  144  159
#> [3,]   84  144  158
estimate_w(new_tab) # wrong!
#> [1] 0.5
Z <- sample_profiles_without_error(n = 10000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z), to012(Z))
tab
#>    
#>        0    1    2
#>   0 2535    0    0
#>   1    0 2498    0
#>   2    0    0 4967
new_tab <- add_errors_Hp_w(tab, w = 0.25)
new_tab
#>      [,1] [,2] [,3]
#> [1,]  897  958  363
#> [2,]  910 1995 1427
#> [3,]  356 1395 1699
estimate_w(new_tab) # ok
#> [1] 0.2529405
```
