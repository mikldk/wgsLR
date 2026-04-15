library(parallel)
options(mc.cores = 8L)

test_that("wT-posterior-distributions-Ha", {
  skip_if_not_installed("cmdstanr")
  
  set.seed(1)
  q <- 0.2
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  wR <- 1/100
  cases <- sample_data_Ha_wTwR(n = 1000, wT = 1/10, wR = wR, p = p)
  xT <- cases$xT[, 1L]
  xR <- cases$xR[, 1L]
  #table(xT, xR)
  
  m_autogen <- wT_posterior_Ha_stan(xT = xT, xR = xR, wR = wR, p = p,
                                    prior_a = 1, 
                                    prior_b = 1,
                                    chains = 2,
                                    iter_warmup = 100,
                                    iter_sampling = 200,
                                    parallel_chains = 2)
  m_autogen
  wT_mean_autogen <- m_autogen$draws("wT") |> c() |> mean()
  expect_equal(wT_mean_autogen, 1/10, tolerance = 1e-1)
  
  
  m_naive <- wT_posterior_Ha_naive(xT = xT, xR = xR, wR = wR, p = p,
                                   prior_a = 1, 
                                   prior_b = 1,
                                   chains = 8,
                                   chains_function = mclapply,
                                   warmup = 10, iterations = 50) 
  #library(ggplot2); ggplot(posterior_samples_df(m_naive), aes(iteration, sample, colour = factor(chain))) + geom_line()
  #m_naive
  wT_mean_naive <- posterior_samples(m_naive)
  expect_equal(mean(wT_mean_naive), 1/10, tolerance = 1e-1)
})

