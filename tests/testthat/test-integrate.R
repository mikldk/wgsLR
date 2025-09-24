test_that("LR components", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- expand.grid(xD = 0L:2L, 
                      xS = 0L:2L)
  
  for (wS in c(1e-8, 1e-4, 1e-2)) {
    for (wD in c(1e-10, 1e-8, 1e-4)) {
      for (i in seq_along(case$xD)) {
        
        x1 <- calc_LR_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        x2 <- calc_LR_num_Hp_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        x3 <- calc_LR_den_Hd_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L])
        
        expect_equal(x1, x2/x3)
      }
    }
  }
})

test_that("exact vs numeric integration", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  
  wS <- 1e-3
  shp1 <- 1
  shp2 <- 9
  
  x1 <- int_Hp_xD0_xS0(wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], a = shp1, b = shp2)
  x2 <- integrate(\(wD) {
    calc_LR_num_Hp_single_no_checks_wDwS(xD = 0L, xS = 0L, wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
      dbeta05(wD, shape1 = shp1, shape2 = shp2)
  }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
  expect_equal(x1, x2, tolerance = 1e-12)
  
  
  x1 <- int_Hp_xD0_xS2(wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], a = shp1, b = shp2)
  x2 <- integrate(\(wD) {
    calc_LR_num_Hp_single_no_checks_wDwS(xD = 0L, xS = 2L, wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
      dbeta05(wD, shape1 = shp1, shape2 = shp2)
  }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
  expect_equal(x1, x2, tolerance = 1e-12)
  
  
  
  case <- expand.grid(xD = 0L:2L, 
                      xS = 0L:2L)
  
  for (wS in c(1e-8, 1e-4, 1e-2)) {
    for (i in seq_along(case$xD)) {
      
      x1p <- int_LR_num_Hp_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], shape1D = shp1, shape2D = shp2)
      
      x2p <- integrate(\(wD) {
        calc_LR_num_Hp_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
          dbeta05(wD, shape1 = shp1, shape2 = shp2)
      }, lower = 0, upper = 0.5, abs.tol = 1e-14, rel.tol = 1e-14)$value
      expect_equal(x1p, x2p, tolerance = 1e-12)
      
      ####
      
      
      x1d <- int_LR_den_Hd_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L], shape1D = shp1, shape2D = shp2)
      
      x2d <- integrate(\(wD) {
        calc_LR_den_Hd_single_no_checks_wDwS(xD = case$xD[i], xS = case$xS[i], wD = wD, wS = wS, p_0 = p[1L], p_1 = p[2L], p_2 = p[3L]) * 
          dbeta05(wD, shape1 = shp1, shape2 = shp2)
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
  # case <- sample_data_Hp_wDwS(20, wD = 1e-3, wS = 1e-4, p = p)
  # dput(case)
  case <- list(xD = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)), 
               xS = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)))
  
  
  wS_mean <- 1e-4
  wS_var <- 1e-9
  wS_shp <- get_beta_parameters(mu = wS_mean, sigmasq = wS_var, a = 0, b = 0.5)
  #curve(dbeta05(x, wS_shp[1], wS_shp[2]), from = 0, to = 1e-3); abline(v = wS_mean)

  unif_LR_wD_mc <- calc_LRs_wDwS_integrate_wD_mc(xD = case$xD, xS = case$xS, 
                                                 shape1D = 1, shape2D = 1, wS = 1e-4, p = p, 
                                                 n_samples = 2000)
  unif_LR_wD_exact <- calc_LRs_wDwS_integrate_wD(xD = case$xD, xS = case$xS, 
                                                 shape1D_H1 = 1, shape2D_H1 = 1,
                                                 shape1D_H2 = 1, shape2D_H2 = 1, 
                                                 wS = 1e-4, p = p)
  
  WoE1 <- sum(log10(unif_LR_wD_mc))
  WoE2 <- sum(log10(unif_LR_wD_exact))
  expect_equal(WoE1, WoE2, tolerance = 1e-1)

  
  
  ############
  unif_LR_wD_num <- calc_LRs_wDwS_integrate_wD_num(xD = case$xD, xS = case$xS, 
                                                   shape1D_H1 = 1, shape2D_H1 = 1,
                                                   shape1D_H2 = 1, shape2D_H2 = 1, 
                                                   wS = 1e-4, p = p, 
                                                   abs.tol = 1e-12, rel.tol = 1e-12, subdivisions = 100000)
  
  WoE3 <- sum(log10(unif_LR_wD_num))
  
  expect_equal(WoE2, WoE3, tolerance = 1e-10)
  
  ############
  
})



test_that("exact vs numeric integration", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- expand.grid(xD = 0L:2L, 
                      xS = 0L:2L,
                      shp1 = c(1),
                      shp2 = c(1000),
                      wS = c(1e-3, 1e-2))
  

  for (i in seq_along(case$xD)) {
    unif_LR_wD_num <- calc_LRs_wDwS_integrate_wD_num(xD = case$xD[i], xS = case$xS[i], 
                                                     shape1D_H1 = case$shp1[i], 
                                                     shape2D_H1 = case$shp2[i],
                                                     shape1D_H2 = case$shp1[i], 
                                                     shape2D_H2 = case$shp2[i], 
                                                     wS = case$wS[i], 
                                                     p = p, 
                                                     abs.tol = 1e-13, rel.tol = 1e-13, subdivisions = 100000)
    
    unif_LR_wD_exact <- calc_LRs_wDwS_integrate_wD(xD = case$xD[i], xS = case$xS[i], 
                                                   shape1D_H1 = case$shp1[i], 
                                                   shape2D_H1 = case$shp2[i],
                                                   shape1D_H2 = case$shp1[i], 
                                                   shape2D_H2 = case$shp2[i], 
                                                   wS = case$wS[i], 
                                                   p = p)
    
    unif_LR_wD_num
    unif_LR_wD_exact
    
    #waldo::compare(unif_LR_wD_num, unif_LR_wD_exact)
    expect_equal(unif_LR_wD_num, unif_LR_wD_exact, tolerance = 1e-10)
  }
  
})



test_that("integration error", {
  q <- 0.9
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- list(xD = structure(c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), dim = c(40L, 1L)), 
               xS = structure(c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), dim = c(40L, 1L)))
  
  
  wS_mean <- 1e-4
  wS_var <- 1e-9
  wS_shp <- get_beta_parameters(mu = wS_mean, sigmasq = wS_var, a = 0, b = 0.5)
  #curve(dbeta05(x, wS_shp[1], wS_shp[2]), from = 0, to = 1e-3); abline(v = wS_mean)
  
  LR_wD_num <- calc_LRs_wDwS_integrate_wD_num(xD = case$xD, xS = case$xS, 
                                              shape1D_H1 = wS_shp[1], shape2D_H1 = wS_shp[2],
                                              shape1D_H2 = wS_shp[1], shape2D_H2 = wS_shp[2], 
                                              wS = 1e-4, p = p)
  LR_wD_num_precise <- calc_LRs_wDwS_integrate_wD_num(xD = case$xD, xS = case$xS, 
                                                      shape1D_H1 = wS_shp[1], shape2D_H1 = wS_shp[2],
                                                      shape1D_H2 = wS_shp[1], shape2D_H2 = wS_shp[2], 
                                                      wS = 1e-4, p = p, 
                                                      abs.tol = 1e-13, rel.tol = 1e-13, subdivisions = 100000)
  WoE3 <- sum(log10(LR_wD_num))
  WoE4 <- sum(log10(LR_wD_num_precise))
  expect_false(isTRUE(all.equal(WoE3, WoE4)))
  ####
  
  LR_wD <- calc_LRs_wDwS_integrate_wD(xD = case$xD, xS = case$xS, 
                                      shape1D_H1 = wS_shp[1], shape2D_H1 = wS_shp[2],
                                      shape1D_H2 = wS_shp[1], shape2D_H2 = wS_shp[2], 
                                      wS = 1e-4, 
                                      p = p)
  #LR_wD
  WoE6 <- sum(log10(LR_wD))
  #WoE6
  expect_equal(WoE4, WoE6, tolerance = 1e-10)

})



test_that("integration both error rates", {
  q <- 0.4
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  
  case <- list(xD = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)), 
               xS = structure(c(1, 1, 2, 0, 1, 0, 0, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2), dim = c(20L, 1L)))
  
  
  wS_mean <- 1e-4
  wS_var <- 1e-9
  wS_shp <- get_beta_parameters(mu = wS_mean, sigmasq = wS_var, a = 0, b = 0.5)
  wS_mean <- 1e-4
  wS_var <- 1e-9
  wS_shp <- get_beta_parameters(mu = wS_mean, sigmasq = wS_var, a = 0, b = 0.5)
  
  # Call to 
  # calc_LRs_wDwS_integrate_wDwS_mc()
})

