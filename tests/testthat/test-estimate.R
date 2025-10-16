test_that("estimate_w", {
  cases <- sample_data_Hp_w(n = 100000, w = 0.3, p = c(0.25, 0.25, 0.5))
  tab <- table(cases$xT, cases$xR)
  w_hat <- estimate_w(tab)
  expect_equal(w_hat, 0.3, tolerance = 0.1)
  
  
  cases <- sample_data_Hp_w(n = 100000, w = 0.2, p = c(0.25, 0.25, 0.5))
  tab <- table(cases$xT, cases$xR)
  w_hat <- estimate_w(tab)
  expect_equal(w_hat, 0.2, tolerance = 0.1)
})


test_that("beta05", {
  shp1 <- 1
  shp2 <- 5
  
  par <- get_beta_mu_sigma(shp1, shp2, a = 0, b = 1)
  expect_equal(par$mean, shp1 / (shp1 + shp2), tolerance = 1e-12)
  
  par <- get_beta_mu_sigma(shp1, shp2, a = 0, b = 0.5)
  expect_equal(par$mean, 0.5*(shp1 / (shp1 + shp2)), tolerance = 1e-12)
  
  shp12 <- get_beta_parameters(par$mean, par$var, a = 0, b = 0.5)
  expect_equal(shp12[1L], shp1, tolerance = 1e-12)
  expect_equal(shp12[2L], shp2, tolerance = 1e-12)
  
  
  x <- rbeta05(100000, shp1, shp2)
  expect_equal(mean(x), par$mean, tolerance = 0.01)
  expect_equal(var(x), par$var, tolerance = 0.01)
})

