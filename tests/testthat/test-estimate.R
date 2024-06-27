test_that("estimate_w", {
  cases <- sample_data_Hp(n = 100000, w = 0.3, p = c(0.25, 0.25, 0.5))
  tab <- table(cases$X_D, cases$X_S)
  w_hat <- estimate_w(tab)
  expect_equal(w_hat, 0.3, tolerance = 0.1)
  
  
  cases <- sample_data_Hp(n = 100000, w = 0.2, p = c(0.25, 0.25, 0.5))
  tab <- table(cases$X_D, cases$X_S)
  w_hat <- estimate_w(tab)
  expect_equal(w_hat, 0.2, tolerance = 0.1)
})
