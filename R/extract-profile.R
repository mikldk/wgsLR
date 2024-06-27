#' Extract profile from ranked markers
#' 
#' Uses the most recent marker ranking (see function argument).
#' 
#' NOTE: This does not check if reference and alternative alleles are the same.
#'       See examples below for one way to inspect this.
#' 
#' @param x Data (potentially already filtered), including columns `chr` and `pos`
#' @param markers Marker ranking
#' 
#' @return Original data with extra columns `segment`, `segment_no` and `rank`
#' @examples
#' vcf <- vcf_example_data_v1 |> rename(chr = CHROM, pos = POS)
#' prof <- extract_profile(vcf)
#' head(prof)
#' nrow(prof)
#' table(prof$rank)
#' 
#' # Also using allele frequencies from `marker_candidates_v1`
#' prof_extra_info <- prof |> inner_join(marker_candidates_v1 |> 
#'   select(chr, pos, ref, alt, AF_afr, AF_eas, AF_nfe), by = c("chr", "pos"))
#' head(prof_extra_info)
#' 
#' subset(prof_extra_info, ref != REF)
#' subset(prof_extra_info, alt != ALT)
#' subset(prof_extra_info, alt != ALT & ALT != "?")
#' 
#' X <- to012(prof_extra_info$GT)
#' X
#' genoprob <- allele_probs_to_geno_probs(prof_extra_info$AF_nfe)
#' 
#' LRs <- calc_LRs(xs = X, xd = X, w = 0.01, p = genoprob)
#' WoE <- sum(log10(LRs))
#' WoE
#' 
#' ws <- 10^(-(6:1))
#' LRs_ws <- sapply(ws, \(w) sum(log10(calc_LRs(xs = X, xd = X, w = w, p = genoprob))))
#' cbind(ws, LRs_ws)
#' 
#' # Assume two errors
#' head(X)
#' Xd <- X
#' Xd[1] <- 1
#' Xd[2] <- 1
#' LRs_ws_Xd <- sapply(ws, \(w) sum(log10(calc_LRs(xs = X, xd = Xd, w = w, p = genoprob))))
#' cbind(ws, LRs_ws_Xd)
#' 
#' # Assume two more errors
#' head(X)
#' Xd[3] <- 0
#' Xd[4] <- 0
#' LRs_ws_Xd4 <- sapply(ws, \(w) sum(log10(calc_LRs(xs = X, xd = Xd, w = w, p = genoprob))))
#' cbind(ws, LRs_ws_Xd4)
#' 
#' @importFrom dplyr select slice_min group_by rename inner_join
#' @export
extract_profile <- function(x, markers = marker_candidates_v1) {
  marker_cand <- markers |> select(chr, pos, segment, segment_no, rank)
  cand <- x |> dplyr::inner_join(marker_cand, by = c("chr", "pos")) 
  cand_rank <- cand |>
    ungroup() |> 
    group_by(chr, segment_no) |>
    slice_min(n = 1L, order_by = rank) |> 
    ungroup()

  cand_rank
}

#' Extract profile 
#' 
#' Uses `marker_candidates_v1` ranking.
#' 
#' @inheritParams extract_profile
#' @inherit extract_profile examples
#' 
#' @export
extract_profile_v1 <- function(x) {
  extract_profile(x, markers = marker_candidates_v1)
}

