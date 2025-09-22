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
  
  LRs <- lapply(seq_len(nrow(Hp_cases$xS)), function(i) {
    calc_LRs_w(xD = Hp_cases$xD[i, ], xS = Hp_cases$xS[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs)) > 0) # hist(log10(LRs))
  expect_true(mean(LRs > 1) > 0.8) # Expect more than 80% to have LR > 1
  
  
  ###
  
  Hd_cases <- sample_data_Hd_w(n = 1000, w = w, p = p)
  LRs <- lapply(seq_len(nrow(Hd_cases$xS)), function(i) {
    calc_LRs_w(xD = Hd_cases$xD[i, ], xS = Hd_cases$xS[i, ], w = w, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs)) < 0) # hist(log10(LRs))
  expect_true(mean(LRs > 1) < 0.2) # Expect less than 20% to have LR > 1
  
  
  ###
})


test_that("LR_wDwS", {
  LRw <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(4, 4))
  expect_equal(LRw, LRwDwS)
  
  LRw <- calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = list(
    c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
  expect_equal(LRwDwS, c(4, 10))
  expect_equal(LRw, LRwDwS)
  
  
  LRw <- calc_LRs_w(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(4, 0))
  expect_equal(LRw, LRwDwS)
  
  ###
  
  p <- reuse_genotype_probs(c(0.25, 0.25, 0.5), 10)
  wD <- 1e-2
  wS <- 1e-6
  Hp_cases <- sample_data_Hp_wDwS(n = 1000, wD = wD, wS = wS, p = p)
  
  LRs_w <- lapply(seq_len(nrow(Hp_cases$xS)), function(i) {
    calc_LRs_w(xD = Hp_cases$xD[i, ], xS = Hp_cases$xS[i, ], w = (wD+wS)/2, p = p)
  }) |> lapply(prod) |> unlist()
  LRs_wDwS <- lapply(seq_len(nrow(Hp_cases$xS)), function(i) {
    calc_LRs_wDwS(xD = Hp_cases$xD[i, ], xS = Hp_cases$xS[i, ], wD = wD, wS = wS, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs_w)) > 0) # hist(log10(LRs_w))
  expect_true(mean(log10(LRs_wDwS)) > 0) # hist(log10(LRs_wDwS))
  expect_true(mean(LRs_wDwS > 1) > 0.8) # Expect more than 80% to have LR > 1
  
  ###
  
  Hd_cases <- sample_data_Hd_wDwS(n = 1000, wD = wD, wS = wS, p = p)
  
  LRs_wDwS <- lapply(seq_len(nrow(Hd_cases$xS)), function(i) {
    calc_LRs_wDwS(xD = Hd_cases$xD[i, ], xS = Hd_cases$xS[i, ], wD = wD, wS = wS, p = p)
  }) |> lapply(prod) |> unlist()
  expect_true(mean(log10(LRs_wDwS)) < 0) # hist(log10(LRs_wDwS))
  expect_true(mean(LRs_wDwS > 1) < 0.2) # Expect less than 20% to have LR > 1
})



test_that("calc_LRs_wDwS_integrate_wD", {
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(4, 4))
  
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(3.95916096870935, 0.040068717630562))
  
  z1 <- calc_LRs_wDwS_integrate_wD_mc(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = 1, shape2D = 1,
    wS = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 10000)
  z2 <- calc_LRs_wDwS_integrate_wD_num(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = 1, shape2D = 1,
    wS = 1e-5,
    p = c(0.25, 0.25, 0.5)) |> 
    lapply(\(x) x$value) |> unlist()
  expect_equal(z2, c(2.54631759119349, 0.731700155636665), tolerance = 1e-8)
  expect_equal(z1, z2, tolerance = 1e-2)
  
  shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-6, a = 0, b = 0.5)
  z1 <- calc_LRs_wDwS_integrate_wD_mc(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = shpD[1], shape2D = shpD[2],
    wS = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 10000)
  z2 <- calc_LRs_wDwS_integrate_wD_num(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = shpD[1], shape2D = shpD[2],
    wS = 1e-5,
    p = c(0.25, 0.25, 0.5)) |> 
    lapply(\(x) x$value) |> unlist()
  expect_equal(z2, c(3.95915297872444, 0.0400684712827476), tolerance = 1e-8)
  expect_equal(z1, z2, tolerance = 1e-2)
  expect_equal(LRwDwS, z1, tolerance = 1e-3)
  expect_equal(LRwDwS, z2, tolerance = 1e-3)
})



test_that("calc_LRs_wDwS_integrate_wDwS", {
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(4, 4))
  
  LRwDwS <- calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwDwS, c(3.95916096870935, 0.040068717630562))
  
  z <- replicate(5, calc_LRs_wDwS_integrate_wDwS_mc(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = 1, shape2D = 1,
    shape1S = 1, shape2S = 1,
    p = c(0.25, 0.25, 0.5),
    n_samples = 100))
  z <- apply(z, 1, mean)
  expect_equal(z, c(1.8, 0.9), tolerance = 1e-1)
  
  
  shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-8, a = 0, b = 0.5)
  shpS <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-8, a = 0, b = 0.5)
  z <- replicate(5, calc_LRs_wDwS_integrate_wDwS_mc(
    xD = c(0, 0),
    xS = c(0, 1),
    shape1D = shpD[1], shape2D = shpD[2],
    shape1S = shpS[1], shape2S = shpS[2],
    p = c(0.25, 0.25, 0.5),
    n_samples = 100))
  z <- apply(z, 1, mean)
  expect_equal(z, c(3.95, 0.04), tolerance = 1e-1)
})
