test_that("LR_w", {
  LR <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LR, c(4, 4))
  
  LR <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  expect_equal(LR, c(4, 10))
  
  LR <- calc_LRs_w(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LR, c(4, 0))
  
  ###
  
  p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 10)
  w <- 1e-2
  Hp_cases <- sample_data_Hp_w(n = 1000, w = w, p = p)
  
  LRs <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_w(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs)) > 0) # hist(log10(LRs))
  expect_true(mean(LRs > 1) > 0.8) # Expect more than 80% to have LR > 1
  
  
  ###
  
  Hd_cases <- sample_data_Hd_w(n = 1000, w = w, p = p)
  LRs <- lapply(seq_len(nrow(Hd_cases$xR)), function(i) {
    calc_LRs_w(xT = Hd_cases$xT[i, ], xR = Hd_cases$xR[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs)) < 0) # hist(log10(LRs))
  expect_true(mean(LRs > 1) < 0.2) # Expect less than 20% to have LR > 1
  
  
  ###
})


test_that("LR_wTwR", {
  LRw <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(4, 4))
  expect_equal(LRw, LRwTwR)
  
  LRw <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  expect_equal(LRwTwR, c(4, 10))
  expect_equal(LRw, LRwTwR)
  
  
  LRw <- calc_LRs_w(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(4, 0))
  expect_equal(LRw, LRwTwR)
  
  ###
  
  p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 10)
  wT <- 1e-2
  wR <- 1e-6
  Hp_cases <- sample_data_Hp_wTwR(n = 1000, wT = wT, wR = wR, p = p)
  
  LRs_w <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_w(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], w = (wT+wR)/2, p = p)
  }) |> lapply(prod) |> unlist()
  LRs_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_wTwR(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], wT = wT, wR = wR, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs_w)) > 0) # hist(log10(LRs_w))
  expect_true(mean(log10(LRs_wTwR)) > 0) # hist(log10(LRs_wTwR))
  expect_true(mean(LRs_wTwR > 1) > 0.8) # Expect more than 80% to have LR > 1
  
  ###
  
  Hd_cases <- sample_data_Hd_wTwR(n = 1000, wT = wT, wR = wR, p = p)
  
  LRs_wTwR <- lapply(seq_len(nrow(Hd_cases$xR)), function(i) {
    calc_LRs_wTwR(xT = Hd_cases$xT[i, ], xR = Hd_cases$xR[i, ], wT = wT, wR = wR, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs_wTwR)) < 0) # hist(log10(LRs_wTwR))
  expect_true(mean(LRs_wTwR > 1) < 0.2) # Expect less than 20% to have LR > 1
})



