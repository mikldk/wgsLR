test_that("LR components", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- expand.grid(xT = 0L:2L, 
                      xR = 0L:2L)
  
  for (wR in c(1e-8, 1e-4, 1e-2)) {
    for (wT in c(1e-10, 1e-8, 1e-4)) {
      for (i in seq_along(case$xT)) {
        
        x1 <- calc_LR_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        x2 <- calc_LR_num_Hp_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        x3 <- calc_LR_den_Ha_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        
        expect_equal(x1, x2/x3)
      }
    }
  }
})

test_that("exact vs numeric integration", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  
  wR <- 1e-3
  shp1 <- 1
  shp2 <- 9
  
  x1 <- int_Hp_xT0_xR0(wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], a = shp1, b = shp2)
  x2 <- integrate(\(wT) {
    calc_LR_num_Hp_single_no_checks_wTwR(xT = 0L, xR = 0L, wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
      dbeta05(wT, shape1 = shp1, shape2 = shp2)
  }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
  expect_equal(x1, x2, tolerance = 1e-12)
  
  
  x1 <- int_Hp_xT0_xR2(wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], a = shp1, b = shp2)
  x2 <- integrate(\(wT) {
    calc_LR_num_Hp_single_no_checks_wTwR(xT = 0L, xR = 2L, wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
      dbeta05(wT, shape1 = shp1, shape2 = shp2)
  }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
  expect_equal(x1, x2, tolerance = 1e-12)
  
  
  
  case <- expand.grid(xT = 0L:2L, 
                      xR = 0L:2L)
  
  for (wR in c(1e-8, 1e-4, 1e-2)) {
    for (i in seq_along(case$xT)) {
      
      x1p <- int_LR_num_Hp_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], shape1T = shp1, shape2T = shp2)
      
      x2p <- integrate(\(wT) {
        calc_LR_num_Hp_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
          dbeta05(wT, shape1 = shp1, shape2 = shp2)
      }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
      expect_equal(x1p, x2p, tolerance = 1e-12)
      
      ####
      
      
      x1d <- int_LR_den_Ha_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], shape1T = shp1, shape2T = shp2)
      
      x2d <- integrate(\(wT) {
        calc_LR_den_Ha_single_no_checks_wTwR(xT = case$xT[i], xR = case$xR[i], wT = wT, wR = wR, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
          dbeta05(wT, shape1 = shp1, shape2 = shp2)
      }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
      expect_equal(x1d, x2d, tolerance = 1e-12)
      
      ####
      
      #cat("P(E | Hp) = ", x1p, "; P(E | Hd) = ", x1d, "; LR = ", (x1p / x1d), "\n")
    }
  }
})





test_that("integration uniform prior", {
  q <- 0.4
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  # set.seed(1)
  # case <- sample_data_Hp_wTwR(20, wT = 1e-3, wR = 1e-4, p = p)
  # dput(case)
  case <- list(xT = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)), 
               xR = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)))
  
  
  unif_WoE_wT_mc <- calc_WoE_wTwR_integrate_wT_mc(xT = case$xT, xR = case$xR, 
                                                  shape1T_Hp = 1, shape2T_Hp = 1,
                                                  shape1T_Ha = 1, shape2T_Ha = 1,
                                                  wR = 1e-4, p = p, 
                                                  n_samples = 10000)
 
  unif_WoE_wT_num <- calc_WoE_wTwR_integrate_wT_num(xT = case$xT, xR = case$xR,
                                                    shape1T_Hp = 1, shape2T_Hp = 1,
                                                    shape1T_Ha = 1, shape2T_Ha = 1,
                                                    wR = 1e-4, p = p)
  
  
  expect_equal(unif_WoE_wT_mc, 8.079683, tolerance = 1e-2)
  expect_equal(unif_WoE_wT_num, 8.079683, tolerance = 1e-2)
  expect_equal(unif_WoE_wT_mc, unif_WoE_wT_num, tolerance = 1e-2)
  
  ############
  
})



test_that("Monte Carlo vs numeric integration", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- expand.grid(xT = 0L:2L, 
                      xR = 0L:2L,
                      shp1 = c(1),
                      shp2 = c(1000),
                      wR = c(1e-3, 1e-2))
  

  for (i in seq_along(case$xT)) {
    unif_LR_wT_num <- calc_WoE_wTwR_integrate_wT_num(xT = case$xT[i], xR = case$xR[i], 
                                                     shape1T_Hp = case$shp1[i], 
                                                     shape2T_Hp = case$shp2[i],
                                                     shape1T_Ha = case$shp1[i], 
                                                     shape2T_Ha = case$shp2[i],
                                                     wR = case$wR[i], 
                                                     p = p)
    
    unif_LR_wT_mc <- calc_WoE_wTwR_integrate_wT_mc(xT = case$xT[i], xR = case$xR[i], 
                                                   shape1T_Hp = case$shp1[i], 
                                                   shape2T_Hp = case$shp2[i],
                                                   shape1T_Ha = case$shp1[i], 
                                                   shape2T_Ha = case$shp2[i],
                                                   wR = case$wR[i], 
                                                   p = p,
                                                   n_samples = 1000)
    
    expect_equal(unif_LR_wT_num, unif_LR_wT_mc, tolerance = 1e-1)
  }
  
})





test_that("calc_WoE_wTwR_integrate_wT", {
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(4, 4))
  
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(3.95916096870935, 0.040068717630562))
  WoE <- sum(log10(LRwTwR))
  expect_equal(WoE, -0.7995914)
  
  shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-6, a = 0, b = 0.5)
  #curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.5)
  
  z1 <- calc_WoE_wTwR_integrate_wT_mc(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = shpT[1], 
    shape2T_Hp = shpT[2],
    shape1T_Ha = shpT[1], 
    shape2T_Ha = shpT[2],
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 10000)
  expect_equal(z1, WoE, tolerance = 1e-3)
  
  z2 <- calc_WoE_wTwR_integrate_wT_num(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = shpT[1], 
    shape2T_Hp = shpT[2],
    shape1T_Ha = shpT[1], 
    shape2T_Ha = shpT[2],
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5))
  expect_equal(z2, WoE, tolerance = 1e-3)
  
  
  ###
  
  z1 <- calc_WoE_wTwR_integrate_wT_mc(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = 1, 
    shape2T_Hp = 1,
    shape1T_Ha = 1, 
    shape2T_Ha = 1,
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 20000)
  
  z2 <- calc_WoE_wTwR_integrate_wT_num(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = 1, 
    shape2T_Hp = 1,
    shape1T_Ha = 1, 
    shape2T_Ha = 1,
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5))
  
  expect_equal(z1, z2, tolerance = 1e-2)
})




test_that("calc_WoE_wTwR_integrate_wT", {
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(4, 4))
  
  LRwTwR <- calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_equal(LRwTwR, c(3.95916096870935, 0.040068717630562))
  WoE <- sum(log10(LRwTwR))
  expect_equal(WoE, -0.7995914)
  
  shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-6, a = 0, b = 0.5)
  #curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.5)
  
  z1 <- calc_WoE_wTwR_integrate_wT_mc(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = shpT[1], shape2T_Hp = shpT[2],
    shape1T_Ha = shpT[1], shape2T_Ha = shpT[2],
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 10000)
  expect_equal(z1, WoE, tolerance = 1e-2)
  
  
  z2 <- calc_WoE_wTwR_integrate_wT_num(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = shpT[1], shape2T_Hp = shpT[2],
    shape1T_Ha = shpT[1], shape2T_Ha = shpT[2],
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5))
  expect_equal(z2, WoE, tolerance = 1e-3)
  
  ###
  
  z1 <- calc_WoE_wTwR_integrate_wT_mc(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = 1, shape2T_Hp = 1,
    shape1T_Ha = 1, shape2T_Ha = 1,
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5),
    n_samples = 20000)

  z2 <- calc_WoE_wTwR_integrate_wT_num(
    xT = c(0, 0),
    xR = c(0, 1),
    shape1T_Hp = 1, shape2T_Hp = 1,
    shape1T_Ha = 1, shape2T_Ha = 1,
    wR = 1e-5,
    p = c(0.25, 0.25, 0.5))

  
  expect_equal(z1, z2, tolerance = 1e-2)
  
})






