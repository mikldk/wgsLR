marker_candidates_v1 <- readRDS("data-raw/s_tidy_af_thres.Rdata")
usethis::use_data(marker_candidates_v1, overwrite = TRUE, version = 3, compress = "bzip2")
