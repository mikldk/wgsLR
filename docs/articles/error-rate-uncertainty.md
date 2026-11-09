# Error rate uncertainty

``` r

library(wgsLR)
```

``` r

q1 <- 0.4
p1 <- c(q1^2, 2*q1*(1-q1), (1-q1)^2)
sum(p1)
#> [1] 1
q2 <- 0.3
p2 <- c(q2^2, 2*q2*(1-q2), (1-q2)^2)
sum(p2)
#> [1] 1
# t: trace sample
# r: reference sample
LRs <- calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 0), wT = 1e-2, wR = 1e-6, p = list(p1, p2))
WoE <- sum(log10(LRs))
WoE
#> [1] 1.808338
```

``` r

shppars <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-3)
shppars
#> [1] 0.078 3.822
curve(dbeta05(x, shppars[1], shppars[2]), from = 0, to = 0.5)
```

![](error-rate-uncertainty_files/figure-html/unnamed-chunk-3-1.png)

``` r

z1 <- calc_WoE_wTwR_integrate_wT_mc(xT = c(0, 0), xR = c(0, 0), 
                                    wR = 1e-6, 
                                    shape1T_Hp = shppars[1],
                                    shape2T_Hp = shppars[2],
                                    shape1T_Ha = shppars[1],
                                    shape2T_Ha = shppars[2],
                                    p = list(p1, p2))
z1
#> [1] 1.80694
z2 <- calc_WoE_wTwR_integrate_wT_num(xT = c(0, 0), xR = c(0, 0), 
                                     wR = 1e-6, 
                                     shape1T_Hp = shppars[1],
                                     shape2T_Hp = shppars[2],
                                     shape1T_Ha = shppars[1],
                                     shape2T_Ha = shppars[2],
                                     p = list(p1, p2))
z2
#> [1] 1.805812
```

Profile likelihood (maximising under each hypothesis), assuming same
genotyping error probability for all markers:

``` r

x <- calc_WoE_wTwR_profilemax_wT_num(
  xT = c(0, 0),
  xR = c(0, 0),
  wR = 1e-6,
  p = list(p1, p2))
x$WoE
#> [1] 1.204115
x$wT_Hp
#> [1] 1e-06
x$wT_Ha
#> [1] 0.5
10^x$log10PrE_Hp
#> [1] 0.01439988
10^x$log10PrE_Ha
#> [1] 0.0009000033
```
