test_that("verify_unimodal", {
  set.seed(20260407)
  
  # Hp cases
  for (rep in 1:10) {
    q <- runif(50, min = 0, max = 1)
    p <- lapply(q, \(w) c(w^2, 2*w*(1-w), (1-w)^2))
    
    wR <- runif(1, min = 1e-8, max = 1e-3)
    wT <- runif(1, min = wR, max = 0.5)
    
    Hp_cases <- sample_data_Hp_wTwR(n = 1, wT = wT, wR = wR, p = p)
    
    xT <- Hp_cases$xT |> c()
    xR <- Hp_cases$xR |> c()
    
    calc_num_Hp <- generate_calc_num_Hp(xT = xT, xR = xR, wR = wR, p = p)
    calc_den_Ha <- generate_calc_den_Ha(xT = xT, xR = xR, wR = wR, p = p)
    
    #calc_num_Hp(1e-3)
    #calc_den_Ha(1e-3)
    
    expect_lte(verify_unimodal(f = calc_num_Hp, lower = 0, upper = 0.5), 1L)
    expect_lte(verify_unimodal(f = calc_den_Ha, lower = 0, upper = 0.5), 1L)
  }
  rm(Hp_cases)

  # Ha cases
  for (rep in 1:10) {
    q <- runif(50, min = 0, max = 1)
    p <- lapply(q, \(w) c(w^2, 2*w*(1-w), (1-w)^2))
    
    wR <- runif(1, min = 1e-8, max = 1e-3)
    wT <- runif(1, min = wR, max = 0.5)
    
    Ha_cases <- sample_data_Ha_wTwR(n = 1, wT = wT, wR = wR, p = p)
    
    xT <- Ha_cases$xT |> c()
    xR <- Ha_cases$xR |> c()
    
    calc_num_Hp <- generate_calc_num_Hp(xT = xT, xR = xR, wR = wR, p = p)
    calc_den_Ha <- generate_calc_den_Ha(xT = xT, xR = xR, wR = wR, p = p)
    
    #calc_num_Hp(1e-3)
    #calc_den_Ha(1e-3)
    
    expect_lte(verify_unimodal(f = calc_num_Hp, lower = 0, upper = 0.5), 1L)
    expect_lte(verify_unimodal(f = calc_den_Ha, lower = 0, upper = 0.5), 1L)
  }
})



test_that("LR_wTwR integrate", {
  set.seed(20260331)
  p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 10)
  wT <- 1e-2
  wR <- 1e-6
  Hp_cases <- sample_data_Hp_wTwR(n = 20, wT = wT, wR = wR, p = p)
  
  WoEs_w <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_w(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], w = (wT+wR)/2, p = p)
  }) |> lapply(log10) |> lapply(sum) |> unlist()
  WoEs_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_wTwR(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], wT = wT, wR = wR, p = p)
  }) |> lapply(log10) |> lapply(sum) |> unlist()
  
  
  shpT <- get_beta_parameters(mu = wT, sigmasq = wT^2/2, a = 0, b = 0.5)
  if (FALSE) {
    curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
    abline(v = wT)
  }
  WoEs_int_mc_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_integrate_wT_mc(xT = Hp_cases$xT[i, ], 
                                  xR = Hp_cases$xR[i, ], 
                                  wR = wR, 
                                  shape1T_Hp = shpT[1],
                                  shape2T_Hp = shpT[2],
                                  shape1T_Ha = shpT[1],
                                  shape2T_Ha = shpT[2],
                                  p = p)
  }) |> unlist()
  WoEs_profilemax_type_optimise_nocheck_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "optimise_nocheck")$WoE
  }) |> unlist()
  WoEs_profilemax_type_optimise_strict_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "optimise_strict")$WoE
  }) |> unlist()
  expect_equal(WoEs_profilemax_type_optimise_nocheck_wTwR, WoEs_profilemax_type_optimise_strict_wTwR)
  WoEs_profilemax_type_optimise_DEoptim_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "DEoptim")$WoE
  }) |> unlist()
  WoEs_mleH2_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_mleH2_wT_num(xT = Hp_cases$xT[i, ], 
                               xR = Hp_cases$xR[i, ], 
                               wR = wR, 
                               p = p)$WoE
  }) |> unlist()
  
  # MLE under H2 must give WoEs that are smaller than the profilemax, both under H1 and H2
  if (FALSE) {
    #idx <- WoEs_profilemax_wTwR <= WoEs_mleH2_wTwR
    idx <- (WoEs_profilemax_wTwR+1e-10) <= WoEs_mleH2_wTwR
    cbind(WoEs_profilemax_wTwR, WoEs_mleH2_wTwR, ifelse(idx, "x", ""))
  }
  expect_true(all(WoEs_mleH2_wTwR < (WoEs_profilemax_type_optimise_nocheck_wTwR + 1e-10)))
  
  # Tests
  expect_true(mean(WoEs_w) > 0) # hist(log10(LRs_w))
  expect_true(mean(WoEs_wTwR) > 0) # hist(log10(LRs_wTwR))
  expect_true(mean(WoEs_wTwR > 1) > 0.8) # Expect more than 80% to have LR > 1
  expect_true(mean(WoEs_int_mc_wTwR) > 0)
  expect_true(mean(WoEs_profilemax_type_optimise_nocheck_wTwR) > 0)
  expect_true(mean(WoEs_profilemax_type_optimise_strict_wTwR) > 0)
  expect_true(mean(WoEs_profilemax_type_optimise_DEoptim_wTwR) > 0)
  expect_true(mean(WoEs_mleH2_wTwR) > 0)
  
  ###
  
  # Same as above, but sample_data_Ha_wTwR instead...
  set.seed(20260331)
  p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 10)
  wT <- 1e-2
  wR <- 1e-6
  Hp_cases <- sample_data_Ha_wTwR(n = 20, wT = wT, wR = wR, p = p)
  
  WoEs_w <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_w(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], w = (wT+wR)/2, p = p)
  }) |> lapply(log10) |> lapply(sum) |> unlist()
  WoEs_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_LRs_wTwR(xT = Hp_cases$xT[i, ], xR = Hp_cases$xR[i, ], wT = wT, wR = wR, p = p)
  }) |> lapply(log10) |> lapply(sum) |> unlist()
  
  
  shpT <- get_beta_parameters(mu = wT, sigmasq = wT^2/2, a = 0, b = 0.5)
  if (FALSE) {
    curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
    abline(v = wT)
  }
  WoEs_int_mc_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_integrate_wT_mc(xT = Hp_cases$xT[i, ], 
                                  xR = Hp_cases$xR[i, ], 
                                  wR = wR, 
                                  shape1T_Hp = shpT[1],
                                  shape2T_Hp = shpT[2],
                                  shape1T_Ha = shpT[1],
                                  shape2T_Ha = shpT[2],
                                  p = p)
  }) |> unlist()
  WoEs_profilemax_type_optimise_nocheck_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "optimise_nocheck")$WoE
  }) |> unlist()
  WoEs_profilemax_type_optimise_strict_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "optimise_strict")$WoE
  }) |> unlist()
  expect_equal(WoEs_profilemax_type_optimise_nocheck_wTwR, WoEs_profilemax_type_optimise_strict_wTwR)
  WoEs_profilemax_type_optimise_DEoptim_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_profilemax_wT_num(xT = Hp_cases$xT[i, ], 
                                    xR = Hp_cases$xR[i, ], 
                                    wR = wR, 
                                    p = p,
                                    opttype = "DEoptim")$WoE
  }) |> unlist()
  WoEs_mleH2_wTwR <- lapply(seq_len(nrow(Hp_cases$xR)), function(i) {
    calc_WoE_wTwR_mleH2_wT_num(xT = Hp_cases$xT[i, ], 
                               xR = Hp_cases$xR[i, ], 
                               wR = wR, 
                               p = p)$WoE
  }) |> unlist()
  
  
  # MLE under H2 must give WoEs that are smaller than the profilemax, both under H1 and H2
  if (FALSE) {
    #idx <- WoEs_profilemax_wTwR <= WoEs_mleH2_wTwR
    idx <- (WoEs_profilemax_wTwR+1e-10) <= WoEs_mleH2_wTwR
    cbind(WoEs_profilemax_wTwR, WoEs_mleH2_wTwR, ifelse(idx, "x", ""))
  }
  expect_true(all(WoEs_mleH2_wTwR < (WoEs_profilemax_type_optimise_nocheck_wTwR + 1e-10)))
  
  # Now, other tests:
  
  expect_true(mean(WoEs_w) < 0) # 
  expect_true(mean(WoEs_wTwR) < 0) # hist(log10(LRs_wTwR))
  expect_true(mean(WoEs_wTwR < 1) > 0.8) # Expect more than 80% to have LR > 1
  expect_true(mean(WoEs_int_mc_wTwR) < 0)
  expect_true(mean(WoEs_profilemax_type_optimise_nocheck_wTwR) < 0)
  expect_true(mean(WoEs_profilemax_type_optimise_strict_wTwR) < 0)
  expect_true(mean(WoEs_profilemax_type_optimise_DEoptim_wTwR) < 0)
  expect_true(mean(WoEs_mleH2_wTwR) < 0)
})

