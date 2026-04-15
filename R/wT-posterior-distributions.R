generate_num_Hp_loglik <- function(xT, xR, wR, p) {
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  loglik <- function(wT) {
    ll <- unlist(lapply(seq_along(xT), \(i) {
      pi <- p[[i]]
      liki <- calc_LR_num_Hp_single_no_checks_wTwR(xT = xT[i], xR = xR[i], wT = wT, wR = wR, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      log(liki)
    }))
    
    sum(ll)
  }
  
  loglik
}

generate_den_Ha_loglik <- function(xT, xR, wR, p) {
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  loglik <- function(wT) {
    ll <- unlist(lapply(seq_along(xT), \(i) {
      pi <- p[[i]]
      liki <- calc_LR_den_Ha_single_no_checks_wTwR(xT = xT[i], xR = xR[i], wT = wT, wR = wR, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      log(liki)
    }))
    
    sum(ll)
  }
  
  loglik
}

#' Sample from posterior distribution of `wT` conditional on Ha using a naive MCMC sampler
#' 
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' @param prior_a First shape parameter for prior beta distribution
#' @param prior_b Second shape parameter for prior beta distribution
#' @param chains Number of chains to run
#' @param chains_function Function to run the chains (fx `lapply` or `parallel::mclapply`)
#' @param warmup Number of warm-up iterations
#' @param iterations Number of iterations (after warm-up)
#' @param \dots Not used
#' 
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
      curve(dnorm(x, mean = 0.1, sd = 5*1e-2), from = 0, to = 0.5)
      new_w <- stats::rnorm(1, mean = old_w, sd = 5*1e-2)
      if (new_w > 0 && new_w < 0.5) {
        return(new_w)
      }
    }
    
    #lwr <- max(0, old_w - 0.001)
    #upr <- min(0.5, old_w + 0.001)
    #stats::runif(1, lwr, upr)
  }
  
  den_Ha_loglik <- generate_den_Ha_loglik(xT = xT, xR = xR, wR = wR, p = p)
  
  log_posterior <- function(w) {
    den_Ha_loglik(w) + dbeta05(w, shape1 = prior_a, shape2 = prior_b, log = TRUE)
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


#' Sample from posterior distribution of `wT` conditional on Ha using Stan
#' @inheritParams wT_posterior_Ha_naive
#' @param \dots Passed to [cmdstanr::sample()]
#' @export
wT_posterior_Ha_stan <- function(xT, xR, wR, p,
                                 prior_a = 1, 
                                 prior_b = 1, 
                                 chains = 4,
                                 show_messages = FALSE,
                                  ...) {
  
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Please install cmdstanr")
  }
  
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  p_0 <- unlist(lapply(seq_along(xT), \(i) p[[i]][1L]))
  p_1 <- unlist(lapply(seq_along(xT), \(i) p[[i]][2L]))
  p_2 <- unlist(lapply(seq_along(xT), \(i) p[[i]][3L]))
  data <- list(N = length(xT), 
               xT = xT, xR = xR, wR = wR, 
               p_0 = p_0, p_1 = p_1, p_2 = p_2, 
               alpha_wT = prior_a, beta_wT = prior_b)
  
  fl_Ha <- system.file("stan", "wT-posterior-distributions-Ha_autogen.stan", package = "wgsLR")
  m_Ha_autogen <- cmdstanr::cmdstan_model(fl_Ha, quiet = TRUE)
  f_Ha_autogen <- m_Ha_autogen$sample(data = data, chains = chains, show_messages = show_messages, ...)
  f_Ha_autogen
}

