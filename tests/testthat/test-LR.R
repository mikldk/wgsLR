test_that("LR", {
  LR <- calc_LRs(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LR, c(4, 4))
  
  LR <- calc_LRs(c(0, 0), c(0, 0), w = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  expect_equal(LR, c(4, 10))
  
  LR <- calc_LRs(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LR, c(4, 0))
  
  ###
  
  p <- list(c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1))
  w <- 1e-2
  Hp_cases <- sample_data_Hp(n = 1000, w = w, p = p)
  
  LRs <- lapply(seq_len(nrow(Hp_cases$X_S)), function(i) {
    calc_LRs(xs = Hp_cases$X_S[i, ], xd = Hp_cases$X_D[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(LRs > 1) > 0.8) # Expect more than 80% to have LR > 1
  
  
  ###
  
  p <- list(c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1))
  w <- 1e-2
  Hd_cases <- sample_data_Hd(n = 1000, w = w, p = p)
  
  LRs <- lapply(seq_len(nrow(Hd_cases$X_S)), function(i) {
    calc_LRs(xs = Hd_cases$X_S[i, ], xd = Hd_cases$X_D[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(LRs > 1) < 0.2) # Expect less than 20% to have LR > 1
})
