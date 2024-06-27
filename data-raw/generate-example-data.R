library(tidyverse)

d <- readRDS("file.Rdata")

d_subset <- d |> 
  semi_join(marker_candidates_v1, by = c("CHROM" = "chr", "POS" = "pos"))

set.seed(20240520)
d_subset_non_part <- d |> 
  anti_join(marker_candidates_v1, by = c("CHROM" = "chr", "POS" = "pos")) |> 
  group_by(CHROM) |> 
  sample_n(100)

vcf_example_data_v1 <- bind_rows(
  d_subset, d_subset_non_part
) |> 
  arrange(CHROM, POS) |> 
  as.data.frame()

if (FALSE) {
  d_subset |> 
    inner_join(marker_candidates_v1, by = c("CHROM" = "chr", "POS" = "pos")) |> 
    count(segment) |> 
    print(n = Inf)
}

usethis::use_data(vcf_example_data_v1, overwrite = TRUE, version = 3, compress = "bzip2")
