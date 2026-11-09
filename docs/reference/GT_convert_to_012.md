# Convert GT field from 0/0, 0/1, 0\|1 etc. to 0, 1, 2

Convert GT field from 0/0, 0/1, 0\|1 etc. to 0, 1, 2

## Usage

``` r
GT_convert_to_012(x)
```

## Examples

``` r
GT_convert_to_012("0/0")
#> [1] 0
GT_convert_to_012("1|0")
#> [1] 1
GT_convert_to_012("1/0")
#> [1] 1
GT_convert_to_012("0/1")
#> [1] 1
GT_convert_to_012("1/1")
#> [1] 2
# GT_convert_to_012("1/2")
```
