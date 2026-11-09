# Add Hp-errors to table for one error probability, \$w\$

Add Hp-errors to table for one error probability, \$w\$

## Usage

``` r
add_errors_Hp_w(tab, w)
```

## Arguments

- tab:

  table to add errors to

- w:

  error probability

## Examples

``` r
Z <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
tab <- table(to012(Z))
tab
#> 
#>   0   1   2 
#> 252 217 531 
new_tab <- add_errors_Hp_w(tab, w = 0.15)
new_tab
#>      [,1] [,2] [,3]
#> [1,]  129   76   12
#> [2,]   66  164  111
#> [3,]   20  125  297
estimate_w(new_tab)
#> [1] 0.1485783
```
