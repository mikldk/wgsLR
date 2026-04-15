library(tidyverse)
library(caracas)



gen_code <- function(d) {
    
  code <- "
  real wTsq  = square(wT);
  real wRsq  = square(wR);

  real wTm1  = wT - 1;
  real wRm1  = wR - 1;
  real wTm1sq = square(wTm1);
  real wRm1sq = square(wRm1);
  
  for (n in 1:N) {
    real p_0sq  = square(p_0[n]);
    real p_1sq  = square(p_1[n]);
    real p_2sq  = square(p_2[n]);
  
    real p_01  = p_0[n]*p_1[n];
    real p_02  = p_0[n]*p_2[n];
    real p_12  = p_1[n]*p_2[n];
      "
  
  simp <- function(ex) {
    ex <- gsub("p_0", "p_0[n]", ex, fixed = TRUE)
    ex <- gsub("p_1", "p_1[n]", ex, fixed = TRUE)
    ex <- gsub("p_2", "p_2[n]", ex, fixed = TRUE)
    
    ex <- gsub("p_0[n]^2", "p_0sq", ex, fixed = TRUE)
    ex <- gsub("p_1[n]^2", "p_1sq", ex, fixed = TRUE)
    ex <- gsub("p_2[n]^2", "p_2sq", ex, fixed = TRUE)
    
    ex <- gsub("(wR - 1)^2", "wRm1sq", ex, fixed = TRUE)
    ex <- gsub("(wT - 1)^2", "wTm1sq", ex, fixed = TRUE)
    
    ex <- gsub("(wR - 1)", "wRm1", ex, fixed = TRUE)
    ex <- gsub("(wT - 1)", "wTm1", ex, fixed = TRUE)
    
    ex <- gsub("p_0[n]*p_1[n]", "p_01", ex, fixed = TRUE)
    ex <- gsub("p_0[n]*p_2[n]", "p_02", ex, fixed = TRUE)
    ex <- gsub("p_1[n]*p_2[n]", "p_12", ex, fixed = TRUE)
    
    ex <- gsub("wR^2", "wRsq", ex, fixed = TRUE)
    ex <- gsub("wT^2", "wTsq", ex, fixed = TRUE)
    
    ex
  }
  
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      #cat("xT <- ", xT, "; xR <- ", xR, "\n", sep = "")
      
      e <- d |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      s <- caracas::as_sym(e)
      ex <- simp(s |> as_character())
      ex <- paste0("log(", ex, ")")

      condition <- paste0("else if (xT[n] == ", xT, " && xR[n] == ", xR, ") ")
      
      if (xT == 0L && xR == 0L) {
        condition <- paste0("if (xT[n] == ", xT, " && xR[n] == ", xR, ") ")
      }
      
      # code <- paste0(code, condition, " {
      #     return(", ex, ");
      #   }\n")
      
      
      code <- paste0(code, condition, " {
          target += ", ex, ";
        }\n")
    }
  }
  
  code <- paste0(code, " 
    else {
      reject(\"Not recognised\");
    }
  }
  ")

  #code
  
  allcode <- paste0("
  functions {
    // ------------------------------------------------------------------
    // 4-parameter Beta log-density
    // ------------------------------------------------------------------
    real beta_4p_lpdf(real y, real alpha, real beta, real lwr, real upr) {
      real x = (y - lwr) / (upr - lwr);
      return beta_lpdf(x | alpha, beta) - log(upr - lwr);
    }
  }
  
  data {
    int<lower=0> N;
    
    // Observed discrete states
    array[N] int<lower=0, upper=2> xR;
    array[N] int<lower=0, upper=2> xT;
  
    // Known quantities
    real<lower=0, upper=0.5> wR;
    array[N] real<lower=0, upper=1> p_0;
    array[N] real<lower=0, upper=1> p_1;
    array[N] real<lower=0, upper=1> p_2;
  
    // Prior hyperparameters for wT
    real<lower=0> alpha_wT;
    real<lower=0> beta_wT;
  }
  
  parameters {
    // Inference target
    real<lower=0, upper=0.5> wT;
  }
  model {
    // ------------------------------------------------------------
    // Prior
    // ------------------------------------------------------------
    target += beta_4p_lpdf(wT | alpha_wT, beta_wT, 0, 0.5);
  
    // ------------------------------------------------------------
    // Likelihood
    // ------------------------------------------------------------
    ", 
    code, 
    "
  }
  ")
  
  allcode
}

code_Hp <- gen_code(d_prob_Hp_wTwR)
code_Ha <- gen_code(d_prob_Ha_wTwR)

cat(code_Hp, file = here::here("inst", "stan", "wT-posterior-distributions-Hp_autogen.stan"))
cat(code_Ha, file = here::here("inst", "stan", "wT-posterior-distributions-Ha_autogen.stan"))

  
  
  library(cmdstanr)
  set.seed(1)
  q <- 0.2
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  wR <- 1/100
  cases <- sample_data_Ha_wTwR(n = 1000, wT = 1/4, wR = wR, p = p)
  xT <- cases$xT[, 1L]
  xR <- cases$xR[, 1L]
  table(xT, xR)

  p <- reuse_genotype_probs(p = p, n = length(xR))
  p_0 <- unlist(lapply(seq_along(xT), \(i) p[[i]][1L]))
  p_1 <- unlist(lapply(seq_along(xT), \(i) p[[i]][2L]))
  p_2 <- unlist(lapply(seq_along(xT), \(i) p[[i]][3L]))
  data <- list(N = length(xT), xT = xT, xR = xR, wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, alpha_wT = 1, beta_wT = 1)
  
  
  m_Ha_autogen <- cmdstan_model(system.file("stan", "wT-posterior-distributions-Ha_autogen.stan", package = "wgsLR"))
  f_Ha_autogen <- m_Ha_autogen$sample(data = data, chains = 4, show_messages = FALSE)
  f_Ha_autogen$draws("wT") |> c() |> mean()
  f_Ha_autogen$draws("wT") |> c() |> median()
  1/(f_Ha_autogen$draws("wT") |> c() |> quantile(c(0.025, 0.975)))
  f_Ha_autogen$draws("wT") |> c() |> hist()
  
  m_Hp_autogen <- cmdstan_model(system.file("stan", "wT-posterior-distributions-Hp_autogen.stan", package = "wgsLR"))
  f_Hp_autogen <- m_Hp_autogen$sample(data = data, chains = 4, show_messages = FALSE)
  f_Hp_autogen$draws("wT") |> c() |> mean()
  f_Hp_autogen$draws("wT") |> c() |> median()
  1/(f_Hp_autogen$draws("wT") |> c() |> quantile(c(0.025, 0.975)))
  f_Hp_autogen$draws("wT") |> c() |> hist()
  
}

if (FALSE) {
  set.seed(1)
  q <- 0.2
  p <- c(q^2, 2*q*(1-q), (1-q)^2)
  wR <- 1/100
  cases <- sample_data_Hp_wTwR(n = 10, wT = 1/10, wR = wR, p = p)
  xT <- cases$xT[, 1L]
  xR <- cases$xR[, 1L]
  table(xT, xR)
  LR <- calc_LRs_wTwR(xT = xT, xR = xR, wT = 1/10, wR = wR, p = p) |> prod()
  LR
  
  num_Hp_lik <- generate_num_Hp_lik(xT = xT, xR = xR, wR = wR, p = p)
  den_Ha_lik <- generate_den_Ha_lik(xT = xT, xR = xR, wR = wR, p = p)
  num_Hp_lik(1/10) / den_Ha_lik(1/10)
}

#' @examples
#' set.seed(1)
#' q <- 0.2
#' p <- c(q^2, 2*q*(1-q), (1-q)^2)
#' wR <- 1/100
#' cases <- sample_data_Hp_wTwR(n = 100, wT = 1/10, wR = wR, p = p)
#' xT <- cases$xT[, 1L]
#' xR <- cases$xR[, 1L]
#' table(xT, xR)
#' LR <- calc_LRs_wTwR(xT = xT, xR = xR, wT = 1/10, wR = wR, p = p) |> log() |> sum() |> exp()
#' LR
#' LR |> log10()
#' \dontrun{
#' # No convergence
#' library(parallel)
#' options(mc.cores = 4)
#' set.seed(2)
#' wT_post_Ha_res <- wT_posterior_Ha_naive(xT = xT, xR = xR, wT = 1/10, wR = wR, p = p, 
#'                                         warmup = 1000, iterations = 1000, 
#'                                         chains = 4,
#'                                         chains_function = mclapply)
#' sapply(wT_post_Ha_res, \(x) x$acceptance_ratio)
#' #library(ggplot2); ggplot(posterior_samples_df(wT_post_Ha_res), aes(iteration, sample, colour = factor(chain))) + geom_line()
#' wT_post_Ha <- posterior_samples(wT_post_Ha_res)
#' mean(wT_post_Ha)
#' LR_post_Ha <- mclapply(wT_post_Ha, \(wT) calc_LRs_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p) |> log() |> sum() |> exp()) |> unlist()
#' quantile(LR_post_Ha, c(0.025, 0.25, 0.5, 0.75, 0.975))
#' quantile(log10(LR_post_Ha), c(0.025, 0.25, 0.5, 0.75, 0.975))
#' }
#' @export
wT_posterior_Ha_naive <- function(xT, xR, wR, p,
                                  prior_a = 1, 
                                  prior_b = 1, 
                                  chains = 4,
                                  chains_function = lapply,
                                  warmup = 1000, iterations = 10000, ...) {
  
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  proposal <- function(old_w) {
    repeat {
      new_w <- stats::rnorm(1, mean = old_w, sd = 1e-2)
      if (new_w > 0 && new_w < 0.5) {
        return(new_w)
      }
    }
    
    #lwr <- max(0, old_w - 0.001)
    #upr <- min(0.5, old_w + 0.001)
    #stats::runif(1, lwr, upr)
  }
  
  den_Ha_lik <- generate_den_Ha_lik(xT = xT, xR = xR, wR = wR, p = p)
  
  log_posterior <- function(w) {
    log(den_Ha_lik(w)) + dbeta05(w, shape1 = prior_a, shape2 = prior_b, log = TRUE)
  }
    
  generate_samples <- function(w_hat_current, n) {
    log_posterior_current <- log_posterior(w_hat_current)
    
    samples <- numeric(n)
    samples_i <- 1L
    acceptances <- 0L
    
    # Warmup
    Us <- stats::runif(n)
    for (iter in seq_len(n)) {
      w_hat_new <- proposal(w_hat_current)
      log_posterior_new <- log_posterior(w_hat_new)
      
      q <- exp(log_posterior_new - log_posterior_current)
      
      if (q >= 1 || Us[iter] <= q) {
        w_hat_current <- w_hat_new
        log_posterior_current <- log_posterior_new
        acceptances <- acceptances + 1L
      }
      
      samples[iter] <- w_hat_current
    }
    
    samples_LR <- lapply(samples, \(wT) calc_LRs_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p) |> log() |> sum() |> exp()) |> unlist()
    
    return(list(samples = samples, samples_LR = samples_LR, acceptance_ratio = acceptances / n))
  }
  
  
  chain_samples <- chains_function(seq_len(chains), function(chain) {
    # Warmup
    w_hat_current <- stats::runif(1, 0, 0.5)
    if (warmup > 0L) {
      samples <- generate_samples(w_hat_current = w_hat_current, n = warmup)
      w_hat_current <- samples$samples[warmup]
      rm(samples) # discard warmup
    }
    
    # Sampling
    samples <- generate_samples(w_hat_current = w_hat_current, n = iterations)
    samples
  })
  
  class(chain_samples) <- c("wgsLR_bayesian_sample", class(chain_samples))
  chain_samples
}

