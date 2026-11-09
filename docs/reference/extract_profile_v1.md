# Extract profile

Uses `marker_candidates_v1` ranking.

## Usage

``` r
extract_profile_v1(x)
```

## Arguments

- x:

  Data (potentially already filtered), including columns `chr` and `pos`

## Examples

``` r
vcf <- vcf_example_data_v1 |> rename(chr = CHROM, pos = POS)
#> Error: object 'vcf_example_data_v1' not found
prof <- extract_profile(vcf)
#> Error: object 'vcf' not found
head(prof)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'prof' not found
nrow(prof)
#> Error: object 'prof' not found
table(prof$rank)
#> Error: object 'prof' not found

# Also using allele frequencies from `marker_candidates_v1`
prof_extra_info <- prof |> inner_join(marker_candidates_v1 |> 
  select(chr, pos, ref, alt, AF_afr, AF_eas, AF_nfe), by = c("chr", "pos"))
#> Error: object 'prof' not found
head(prof_extra_info)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'prof_extra_info' not found

subset(prof_extra_info, ref != REF)
#> Error: object 'prof_extra_info' not found
subset(prof_extra_info, alt != ALT)
#> Error: object 'prof_extra_info' not found
subset(prof_extra_info, alt != ALT & ALT != "?")
#> Error: object 'prof_extra_info' not found

X <- to012(prof_extra_info$GT)
#> Error: object 'prof_extra_info' not found
X
#> Error: object 'X' not found
genoprob <- allele_probs_to_geno_probs(prof_extra_info$AF_nfe)
#> Error: object 'prof_extra_info' not found

LRs <- calc_LRs(xT = X, xR = X, w = 0.01, p = genoprob)
#> Error in calc_LRs(xT = X, xR = X, w = 0.01, p = genoprob): could not find function "calc_LRs"
WoE <- sum(log10(LRs))
#> Error: object 'LRs' not found
WoE
#> Error: object 'WoE' not found

ws <- 10^(-(6:1))
LRs_ws <- sapply(ws, \(w) sum(log10(calc_LRs(xT = X, xR = X, w = w, p = genoprob))))
#> Error in calc_LRs(xT = X, xR = X, w = w, p = genoprob): could not find function "calc_LRs"
cbind(ws, LRs_ws)
#> Error: object 'LRs_ws' not found

# Assume two errors
head(X)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'X' not found
XD <- X
#> Error: object 'X' not found
XD[1] <- 1
#> Error: object 'XD' not found
XD[2] <- 1
#> Error: object 'XD' not found
LRs_ws_Xd <- sapply(ws, \(w) sum(log10(calc_LRs(xT = XD, xR = X, w = w, p = genoprob))))
#> Error in calc_LRs(xT = XD, xR = X, w = w, p = genoprob): could not find function "calc_LRs"
cbind(ws, LRs_ws_Xd)
#> Error: object 'LRs_ws_Xd' not found

# Assume two more errors
head(X)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'head': object 'X' not found
XD[3] <- 0
#> Error: object 'XD' not found
XD[4] <- 0
#> Error: object 'XD' not found
LRs_ws_Xd4 <- sapply(ws, \(w) sum(log10(calc_LRs(xT = XD, xR = X, w = w, p = genoprob))))
#> Error in calc_LRs(xT = XD, xR = X, w = w, p = genoprob): could not find function "calc_LRs"
cbind(ws, LRs_ws_Xd4)
#> Error: object 'LRs_ws_Xd4' not found
```
