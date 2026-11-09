# Add Hp-errors to table

Add Hp-errors to table

## Usage

``` r
add_errors_Hp(tab, w)
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
#> 372 130 498 
new_tab <- add_errors_Hp(tab, w = 0.15)
new_tab
#>      [,1] [,2] [,3]
#> [1,]  209   82   15
#> [2,]   79  126  106
#> [3,]   23  116  244
estimate_w(new_tab)
#> [1] 0.157036
```
