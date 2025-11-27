
# `wgsLR`: Shotgun sequencing for human identification: Dynamic SNP marker sets and likelihood ratio calculations accounting for errors

Please refer to the online documentation at
<https://mikldk.github.io/wgsLR>, including the vignettes.

## Scientific publication

The research associated with this software is described in

> Andersen, M. M., Kampmann, M.-L., Jepsen, A. H., Morling, N., Eriksen,
> P. S., Børsting, C., & Andersen, J. D. (2025). *Shotgun DNA sequencing
> for human identification: Dynamic SNP selection and likelihood ratio
> calculations accounting for errors.* Forensic Science International:
> Genetics, 74, 103146.
> [doi:10.1016/j.fsigen.2024.103146](https://doi.org/10.1016/j.fsigen.2024.103146).

## Installation

To install from Github with vignettes run this command from within `R`
(please install `remotes` first if not already installed):

    # install.packages('remotes')
    remotes::install_github("mikldk/wgsLR", build_vignettes = TRUE)

You can also install the package without vignettes if needed as follows:

    remotes::install_github("mikldk/wgsLR")

## A few small examples

### Estimating the genotype error probability, $w$

``` r
cases <- wgsLR::sample_data_Hp_w(n = 1000, w = 0.1, p = c(0.25, 0.25, 0.5))
tab <- table(cases$xT, cases$xR)
tab
```

    ##    
    ##       0   1   2
    ##   0 145  51   8
    ##   1  52 200  86
    ##   2   9  89 360

``` r
w_mle <- wgsLR::estimate_w(tab)
w_mle
```

    ## [1] 0.09417128

#### Cautionary note: not just standard VCF files

It is necessary to obtain sequencing results for all bases in the
selected segments (including read depth and genotype quality). Thus, it
is **not** sufficient to just use information from “confirmed”/high
probability variants from the reference genome (variants identified in
standard vcf-file format), as this can introduce bias in the results.
**Information from all bases in the chosen genomic areas of interest is
needed**. One way to achieve this by using [GATK
HaplotypeCaller](https://gatk.broadinstitute.org/hc/en-us/articles/9570334998171-HaplotypeCaller)
with the additional argument `--emit-ref-confidence BP_RESOLUTION` for
the genomic areas of interest (using `-L areas.interval_list`).

### Calculating likelihood ratios ($LR$’s)

Same genotyping error probability for both trace sample ($x_t =$ `xT`)
and reference sample ($x_r =$ `xR`):

``` r
tab <- wgsLR::d_LR_w[, c("xT", "xR")]
tab$LR <- paste0("$", wgsLR::d_LR_w$expr_tex, "$")
tab |> 
  kbl(format = "markdown")
```

| xT | xR | LR |
|---:|---:|:---|
| 0 | 0 | $\frac{p_{0} \left(1 - w\right)^{4} + p_{1} w^{2} \left(1 - w\right)^{2} + p_{2} w^{4}}{p_{0}^{2} \left(1 - w\right)^{4} + 2 p_{0} p_{1} w \left(1 - w\right)^{3} + 2 p_{0} p_{2} w^{2} \left(1 - w\right)^{2} + p_{1}^{2} w^{2} \left(1 - w\right)^{2} + 2 p_{1} p_{2} w^{3} \left(1 - w\right) + p_{2}^{2} w^{4}}$ |
| 0 | 1 | $\frac{2 p_{0} w \left(1 - w\right)^{3} + p_{1} w^{3} \left(1 - w\right) + p_{1} w \left(1 - w\right)^{3} + 2 p_{2} w^{3} \left(1 - w\right)}{2 p_{0}^{2} w \left(1 - w\right)^{3} + 3 p_{0} p_{1} w^{2} \left(1 - w\right)^{2} + p_{0} p_{1} \left(1 - w\right)^{4} + 2 p_{0} p_{2} w^{3} \left(1 - w\right) + 2 p_{0} p_{2} w \left(1 - w\right)^{3} + p_{1}^{2} w^{3} \left(1 - w\right) + p_{1}^{2} w \left(1 - w\right)^{3} + p_{1} p_{2} w^{4} + 3 p_{1} p_{2} w^{2} \left(1 - w\right)^{2} + 2 p_{2}^{2} w^{3} \left(1 - w\right)}$ |
| 0 | 2 | $\frac{p_{0} w^{2} \left(1 - w\right)^{2} + p_{1} w^{2} \left(1 - w\right)^{2} + p_{2} w^{2} \left(1 - w\right)^{2}}{p_{0}^{2} w^{2} \left(1 - w\right)^{2} + p_{0} p_{1} w^{3} \left(1 - w\right) + p_{0} p_{1} w \left(1 - w\right)^{3} + p_{0} p_{2} w^{4} + p_{0} p_{2} \left(1 - w\right)^{4} + p_{1}^{2} w^{2} \left(1 - w\right)^{2} + p_{1} p_{2} w^{3} \left(1 - w\right) + p_{1} p_{2} w \left(1 - w\right)^{3} + p_{2}^{2} w^{2} \left(1 - w\right)^{2}}$ |
| 1 | 0 | $\frac{2 p_{0} w \left(1 - w\right)^{3} + p_{1} w^{3} \left(1 - w\right) + p_{1} w \left(1 - w\right)^{3} + 2 p_{2} w^{3} \left(1 - w\right)}{2 p_{0}^{2} w \left(1 - w\right)^{3} + 3 p_{0} p_{1} w^{2} \left(1 - w\right)^{2} + p_{0} p_{1} \left(1 - w\right)^{4} + 2 p_{0} p_{2} w^{3} \left(1 - w\right) + 2 p_{0} p_{2} w \left(1 - w\right)^{3} + p_{1}^{2} w^{3} \left(1 - w\right) + p_{1}^{2} w \left(1 - w\right)^{3} + p_{1} p_{2} w^{4} + 3 p_{1} p_{2} w^{2} \left(1 - w\right)^{2} + 2 p_{2}^{2} w^{3} \left(1 - w\right)}$ |
| 1 | 1 | $\frac{4 p_{0} w^{2} \left(1 - w\right)^{2} + p_{1} w^{4} + 2 p_{1} w^{2} \left(1 - w\right)^{2} + p_{1} \left(1 - w\right)^{4} + 4 p_{2} w^{2} \left(1 - w\right)^{2}}{4 p_{0}^{2} w^{2} \left(1 - w\right)^{2} + 4 p_{0} p_{1} w^{3} \left(1 - w\right) + 4 p_{0} p_{1} w \left(1 - w\right)^{3} + 8 p_{0} p_{2} w^{2} \left(1 - w\right)^{2} + p_{1}^{2} w^{4} + 2 p_{1}^{2} w^{2} \left(1 - w\right)^{2} + p_{1}^{2} \left(1 - w\right)^{4} + 4 p_{1} p_{2} w^{3} \left(1 - w\right) + 4 p_{1} p_{2} w \left(1 - w\right)^{3} + 4 p_{2}^{2} w^{2} \left(1 - w\right)^{2}}$ |
| 1 | 2 | $\frac{2 p_{0} w^{3} \left(1 - w\right) + p_{1} w^{3} \left(1 - w\right) + p_{1} w \left(1 - w\right)^{3} + 2 p_{2} w \left(1 - w\right)^{3}}{2 p_{0}^{2} w^{3} \left(1 - w\right) + p_{0} p_{1} w^{4} + 3 p_{0} p_{1} w^{2} \left(1 - w\right)^{2} + 2 p_{0} p_{2} w^{3} \left(1 - w\right) + 2 p_{0} p_{2} w \left(1 - w\right)^{3} + p_{1}^{2} w^{3} \left(1 - w\right) + p_{1}^{2} w \left(1 - w\right)^{3} + 3 p_{1} p_{2} w^{2} \left(1 - w\right)^{2} + p_{1} p_{2} \left(1 - w\right)^{4} + 2 p_{2}^{2} w \left(1 - w\right)^{3}}$ |
| 2 | 0 | $\frac{p_{0} w^{2} \left(1 - w\right)^{2} + p_{1} w^{2} \left(1 - w\right)^{2} + p_{2} w^{2} \left(1 - w\right)^{2}}{p_{0}^{2} w^{2} \left(1 - w\right)^{2} + p_{0} p_{1} w^{3} \left(1 - w\right) + p_{0} p_{1} w \left(1 - w\right)^{3} + p_{0} p_{2} w^{4} + p_{0} p_{2} \left(1 - w\right)^{4} + p_{1}^{2} w^{2} \left(1 - w\right)^{2} + p_{1} p_{2} w^{3} \left(1 - w\right) + p_{1} p_{2} w \left(1 - w\right)^{3} + p_{2}^{2} w^{2} \left(1 - w\right)^{2}}$ |
| 2 | 1 | $\frac{2 p_{0} w^{3} \left(1 - w\right) + p_{1} w^{3} \left(1 - w\right) + p_{1} w \left(1 - w\right)^{3} + 2 p_{2} w \left(1 - w\right)^{3}}{2 p_{0}^{2} w^{3} \left(1 - w\right) + p_{0} p_{1} w^{4} + 3 p_{0} p_{1} w^{2} \left(1 - w\right)^{2} + 2 p_{0} p_{2} w^{3} \left(1 - w\right) + 2 p_{0} p_{2} w \left(1 - w\right)^{3} + p_{1}^{2} w^{3} \left(1 - w\right) + p_{1}^{2} w \left(1 - w\right)^{3} + 3 p_{1} p_{2} w^{2} \left(1 - w\right)^{2} + p_{1} p_{2} \left(1 - w\right)^{4} + 2 p_{2}^{2} w \left(1 - w\right)^{3}}$ |
| 2 | 2 | $\frac{p_{0} w^{4} + p_{1} w^{2} \left(1 - w\right)^{2} + p_{2} \left(1 - w\right)^{4}}{p_{0}^{2} w^{4} + 2 p_{0} p_{1} w^{3} \left(1 - w\right) + 2 p_{0} p_{2} w^{2} \left(1 - w\right)^{2} + p_{1}^{2} w^{2} \left(1 - w\right)^{2} + 2 p_{1} p_{2} w \left(1 - w\right)^{3} + p_{2}^{2} \left(1 - w\right)^{4}}$ |

Sample-specific genotyping error probabilities $w_t$ and $w_r$ for trace
sample ($x_t =$ `xT`) and reference sample ($x_r =$ `xR`), repectively:

``` r
tab <- wgsLR::d_LR_wTwR[, c("xT", "xR")]
tab$LR <- paste0("$", wgsLR::d_LR_wTwR$expr_tex, "$")
tab |> 
  kbl(format = "markdown")
```

| xT | xR | LR |
|---:|---:|:---|
| 0 | 0 | $\frac{p_{0} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + p_{2} w_r^{2} w_t^{2}}{p_{0}^{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} - p_{0} p_{1} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} - p_{0} p_{1} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{0} p_{2} w_r^{2} \left(w_t - 1\right)^{2} + p_{0} p_{2} w_t^{2} \left(w_r - 1\right)^{2} + p_{1}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_r^{2} w_t \left(w_t - 1\right) - p_{1} p_{2} w_r w_t^{2} \left(w_r - 1\right) + p_{2}^{2} w_r^{2} w_t^{2}}$ |
| 0 | 1 | $\frac{2 p_{0} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + p_{1} w_r^{2} w_t \left(w_t - 1\right) + p_{1} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + 2 p_{2} w_r w_t^{2} \left(w_r - 1\right)}{2 p_{0}^{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} - p_{0} p_{1} w_r^{2} \left(w_t - 1\right)^{2} - 2 p_{0} p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{0} p_{1} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + 2 p_{0} p_{2} w_r w_t^{2} \left(w_r - 1\right) + 2 p_{0} p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + p_{1}^{2} w_r^{2} w_t \left(w_t - 1\right) + p_{1}^{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) - p_{1} p_{2} w_r^{2} w_t^{2} - 2 p_{1} p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_t^{2} \left(w_r - 1\right)^{2} + 2 p_{2}^{2} w_r w_t^{2} \left(w_r - 1\right)}$ |
| 0 | 2 | $\frac{p_{0} w_r^{2} \left(w_t - 1\right)^{2} + p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + p_{2} w_t^{2} \left(w_r - 1\right)^{2}}{p_{0}^{2} w_r^{2} \left(w_t - 1\right)^{2} - p_{0} p_{1} w_r^{2} w_t \left(w_t - 1\right) - p_{0} p_{1} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + p_{0} p_{2} w_r^{2} w_t^{2} + p_{0} p_{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + p_{1}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_r w_t^{2} \left(w_r - 1\right) - p_{1} p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{2}^{2} w_t^{2} \left(w_r - 1\right)^{2}}$ |
| 1 | 0 | $\frac{2 p_{0} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{1} w_r w_t^{2} \left(w_r - 1\right) + p_{1} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + 2 p_{2} w_r^{2} w_t \left(w_t - 1\right)}{2 p_{0}^{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) - 2 p_{0} p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{0} p_{1} w_t^{2} \left(w_r - 1\right)^{2} - p_{0} p_{1} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + 2 p_{0} p_{2} w_r^{2} w_t \left(w_t - 1\right) + 2 p_{0} p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{1}^{2} w_r w_t^{2} \left(w_r - 1\right) + p_{1}^{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} - p_{1} p_{2} w_r^{2} w_t^{2} - p_{1} p_{2} w_r^{2} \left(w_t - 1\right)^{2} - 2 p_{1} p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + 2 p_{2}^{2} w_r^{2} w_t \left(w_t - 1\right)}$ |
| 1 | 1 | $\frac{- 4 p_{0} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} w_r^{2} w_t^{2} - p_{1} w_r^{2} \left(w_t - 1\right)^{2} - p_{1} w_t^{2} \left(w_r - 1\right)^{2} - p_{1} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} - 4 p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right)}{- 4 p_{0}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + 2 p_{0} p_{1} w_r^{2} w_t \left(w_t - 1\right) + 2 p_{0} p_{1} w_r w_t^{2} \left(w_r - 1\right) + 2 p_{0} p_{1} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + 2 p_{0} p_{1} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) - 8 p_{0} p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1}^{2} w_r^{2} w_t^{2} - p_{1}^{2} w_r^{2} \left(w_t - 1\right)^{2} - p_{1}^{2} w_t^{2} \left(w_r - 1\right)^{2} - p_{1}^{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + 2 p_{1} p_{2} w_r^{2} w_t \left(w_t - 1\right) + 2 p_{1} p_{2} w_r w_t^{2} \left(w_r - 1\right) + 2 p_{1} p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + 2 p_{1} p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) - 4 p_{2}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right)}$ |
| 1 | 2 | $\frac{2 p_{0} w_r^{2} w_t \left(w_t - 1\right) + p_{1} w_r w_t^{2} \left(w_r - 1\right) + p_{1} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + 2 p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right)}{2 p_{0}^{2} w_r^{2} w_t \left(w_t - 1\right) - p_{0} p_{1} w_r^{2} w_t^{2} - p_{0} p_{1} w_r^{2} \left(w_t - 1\right)^{2} - 2 p_{0} p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + 2 p_{0} p_{2} w_r^{2} w_t \left(w_t - 1\right) + 2 p_{0} p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{1}^{2} w_r w_t^{2} \left(w_r - 1\right) + p_{1}^{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} - 2 p_{1} p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_t^{2} \left(w_r - 1\right)^{2} - p_{1} p_{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + 2 p_{2}^{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right)}$ |
| 2 | 0 | $\frac{p_{0} w_t^{2} \left(w_r - 1\right)^{2} + p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + p_{2} w_r^{2} \left(w_t - 1\right)^{2}}{p_{0}^{2} w_t^{2} \left(w_r - 1\right)^{2} - p_{0} p_{1} w_r w_t^{2} \left(w_r - 1\right) - p_{0} p_{1} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{0} p_{2} w_r^{2} w_t^{2} + p_{0} p_{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + p_{1}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_r^{2} w_t \left(w_t - 1\right) - p_{1} p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + p_{2}^{2} w_r^{2} \left(w_t - 1\right)^{2}}$ |
| 2 | 1 | $\frac{2 p_{0} w_r w_t^{2} \left(w_r - 1\right) + p_{1} w_r^{2} w_t \left(w_t - 1\right) + p_{1} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + 2 p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2}}{2 p_{0}^{2} w_r w_t^{2} \left(w_r - 1\right) - p_{0} p_{1} w_r^{2} w_t^{2} - 2 p_{0} p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{0} p_{1} w_t^{2} \left(w_r - 1\right)^{2} + 2 p_{0} p_{2} w_r w_t^{2} \left(w_r - 1\right) + 2 p_{0} p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} + p_{1}^{2} w_r^{2} w_t \left(w_t - 1\right) + p_{1}^{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) - p_{1} p_{2} w_r^{2} \left(w_t - 1\right)^{2} - 2 p_{1} p_{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2} + 2 p_{2}^{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2}}$ |
| 2 | 2 | $\frac{p_{0} w_r^{2} w_t^{2} + p_{1} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) + p_{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2}}{p_{0}^{2} w_r^{2} w_t^{2} - p_{0} p_{1} w_r^{2} w_t \left(w_t - 1\right) - p_{0} p_{1} w_r w_t^{2} \left(w_r - 1\right) + p_{0} p_{2} w_r^{2} \left(w_t - 1\right)^{2} + p_{0} p_{2} w_t^{2} \left(w_r - 1\right)^{2} + p_{1}^{2} w_r w_t \left(w_r - 1\right) \left(w_t - 1\right) - p_{1} p_{2} w_r \left(w_r - 1\right) \left(w_t - 1\right)^{2} - p_{1} p_{2} w_t \left(w_r - 1\right)^{2} \left(w_t - 1\right) + p_{2}^{2} \left(w_r - 1\right)^{2} \left(w_t - 1\right)^{2}}$ |

#### Example

Assume that a trace sample had four loci with genotypes (0/1 = 1, 0/0 =
0, 1/1 = 2, 1/1 = 2). A person of interest is then typed for the same
four loci and has genotypes (0/0 = 0, 0/0 = 0, 1/1 = 2, 1/1 = 2), i.e. a
mismatch on the first locus.

For simplicity, assume that the genotype probabilites are P(0/0 = 0) =
0.25, P(0/1 = 1/0 = 1) = 0.25, and P(1/1 = 2) = 0.5.

If no errors are possible, then $w=0$ and

``` r
wgsLR::calc_LRs_w(xT = c(1, 0, 2, 2),
                  xR = c(0, 0, 2, 2), 
                  w = 0, 
                  p = c(0.25, 0.25, 0.5))
```

    ## [1] 0 4 2 2

and the product is 0 due to the mismatch at the first locus.

If instead we acknowledge that errors are possible, then for $w = 0.001$
we obtain that

``` r
LR_contribs <- wgsLR::calc_LRs_w(xT = c(1, 0, 2, 2), 
                                 xR = c(0, 0, 2, 2), 
                                 w = 0.001, 
                                 p = c(0.25, 0.25, 0.5))
LR_contribs
```

    ## [1] 0.01192834 3.99199202 1.99799850 1.99799850

``` r
prod(LR_contribs)
```

    ## [1] 0.1900904

We can also consider the $LR$s for a range for plausible values of $w$:

``` r
ws <- c(1e-6, 1e-3, 1e-2, 1e-1)
LRs <- sapply(ws, \(w) wgsLR::calc_LRs_w(xT = c(0, 0, 2, 2), 
                                         xR = c(1, 0, 2, 2), 
                                         w = w, 
                                         p = c(0.25, 0.25, 0.5)) |> 
                prod())
data.frame(log10w = log10(ws), w = ws, 
           LR = LRs, WoElog10LR = log10(LRs))
```

    ##   log10w     w           LR WoElog10LR
    ## 1     -6 1e-06 0.0001919981 -3.7167031
    ## 2     -3 1e-03 0.1900903523 -0.7210399
    ## 3     -2 1e-02 1.7379421372  0.2400353
    ## 4     -1 1e-01 7.1409934157  0.8537586

## Different error rates

Assume that the trace donor profile has $w_D = 10^{-4}$ and the suspect
reference profile has $w_S = 10^{-8}$. Then the $LR$ is:

``` r
LR_contribs <- wgsLR::calc_LRs_wTwR(xT = c(1, 0, 2, 2), 
                                    xR = c(0, 0, 2, 2), 
                                    wT = 1e-4, 
                                    wR = 1e-8,
                                    p = c(0.25, 0.25, 0.5))
prod(LR_contribs)
```

    ## [1] 0.01279168
