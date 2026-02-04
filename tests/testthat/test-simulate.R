test_that("sample_profiles_without_error", {
  Z <- sample_profiles_without_error(n = 10, p = list(
    c(0.25, 0.25, 0.5), 
    c(0.01, 0.01, 0.98), 
    c(0.01, 0.01, 0.98)))
  
  expect_equal(length(Z), 3L) # 3 loci
  expect_equal(ncol(Z[[1]]), 2L) # 2 alleles
  expect_equal(nrow(Z[[1]]), 10L) # 10 individuals
})


test_that("to012", {
  Z <- sample_profiles_without_error(n = 10, p = list(
    c(0.25, 0.25, 0.5), 
    c(0.01, 0.01, 0.98), 
    c(0.01, 0.01, 0.98)))
  
  expect_equal(length(Z), 3L) # 3 loci
  expect_equal(ncol(Z[[1]]), 2L) # 2 alleles
  expect_equal(nrow(Z[[1]]), 10L) # 10 individuals
  
  Z012 <- to012(Z)
  expect_equal(ncol(Z012), 3L) # 3 loci
  expect_equal(nrow(Z012), 10L) # 10 individuals
})


test_that("add_errors_to_genotypes", {
  Z <- sample_profiles_without_error(n = 10, p = list(
    c(0.25, 0.25, 0.5), 
    c(0.01, 0.01, 0.98), 
    c(0.01, 0.01, 0.98)))
  
  expect_equal(length(Z), 3L) # 3 loci
  expect_equal(ncol(Z[[1]]), 2L) # 2 alleles
  expect_equal(nrow(Z[[1]]), 10L) # 10 individuals
  
  X <- add_errors_to_genotypes(Z, 0.5)
  expect_equal(length(Z), length(X)) # 3 loci
  expect_equal(ncol(Z[[1]]), ncol(X[[1]])) # 2 alleles
  expect_equal(nrow(Z[[1]]), nrow(X[[1]])) # 10 individuals
  
  Z012 <- to012(Z)
  X012 <- to012(X)
  expect_equal(ncol(Z012), 3L) # 3 loci
  expect_equal(nrow(Z012), 10L) # 10 individuals
  
  expect_equal(ncol(X012), 3L) # 3 loci
  expect_equal(nrow(X012), 10L) # 10 individuals
  
  expect_equal(dim(Z012), dim(X012))
})

test_that("sample_data_Hp_w", {
  cases <- sample_data_Hp_w(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
  expect_true(is.list(cases))
  expect_equal(names(cases), c("xT", "xR"))
  
  expect_equal(sort(unique(c(cases$xT, cases$xR))), c(0L, 1L, 2L))
  
  expect_equal(ncol(cases$xT), 1L) # 1 locus
  expect_equal(nrow(cases$xT), 1000L) # 1000 individuals
  
  expect_equal(ncol(cases$xR), 1L) # 1 locus
  expect_equal(nrow(cases$xR), 1000L) # 1000 individuals
  
  tab <- table(cases$xT, cases$xR)
  expect_equal(dim(tab), c(3L, 3L))
  
  ####
  
  cases <- sample_data_Hp_w(n = 1000, w = 0, p = c(0.25, 0.25, 0.5))
  expect_true(all(cases$xT == cases$xR))
})

test_that("sample_data_Hp_wTwR", {
  cases <- sample_data_Hp_wTwR(n = 1000, wT = 0.3, wR = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_true(is.list(cases))
  expect_equal(names(cases), c("xT", "xR"))
  
  expect_equal(sort(unique(c(cases$xT, cases$xR))), c(0L, 1L, 2L))
  
  expect_equal(ncol(cases$xT), 1L) # 1 locus
  expect_equal(nrow(cases$xT), 1000L) # 1000 individuals
  
  expect_equal(ncol(cases$xR), 1L) # 1 locus
  expect_equal(nrow(cases$xR), 1000L) # 1000 individuals
  
  tab <- table(cases$xT, cases$xR)
  expect_equal(dim(tab), c(3L, 3L))
  
  ####
  
  cases <- sample_data_Hp_wTwR(n = 1000, wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_true(all(cases$xT == cases$xR))
})





test_that("sample_data_Ha_w", {
  cases <- sample_data_Ha_w(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
  expect_true(is.list(cases))
  expect_equal(names(cases), c("xT", "xR"))
  
  expect_equal(sort(unique(c(cases$xT, cases$xR))), c(0L, 1L, 2L))
  
  expect_equal(ncol(cases$xT), 1L) # 1 locus
  expect_equal(nrow(cases$xT), 1000L) # 1000 individuals
  
  expect_equal(ncol(cases$xR), 1L) # 1 locus
  expect_equal(nrow(cases$xR), 1000L) # 1000 individuals
  
  tab <- table(cases$xT, cases$xR)
  expect_equal(dim(tab), c(3L, 3L))
  
  ####
  
  cases <- sample_data_Ha_w(n = 1000, w = 0, p = c(0.25, 0.25, 0.5))
  expect_false(all(cases$xT == cases$xR))
})


test_that("sample_data_Ha_wTwR", {
  cases <- sample_data_Ha_wTwR(n = 1000, wT = 0.3, wR = 1e-5, p = c(0.25, 0.25, 0.5))
  expect_true(is.list(cases))
  expect_equal(names(cases), c("xT", "xR"))
  
  expect_equal(sort(unique(c(cases$xT, cases$xR))), c(0L, 1L, 2L))
  
  expect_equal(ncol(cases$xT), 1L) # 1 locus
  expect_equal(nrow(cases$xT), 1000L) # 1000 individuals
  
  expect_equal(ncol(cases$xR), 1L) # 1 locus
  expect_equal(nrow(cases$xR), 1000L) # 1000 individuals
  
  tab <- table(cases$xT, cases$xR)
  expect_equal(dim(tab), c(3L, 3L))
  
  ####
  
  cases <- sample_data_Ha_wTwR(n = 1000, wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
  expect_false(all(cases$xT == cases$xR))
})
