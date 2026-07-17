if (FALSE) {
  # Generate code for calc_LR_single_no_checks()
  
  library(tidyverse)
  library(caracas)
  d_tmp <- d_formulas_Hp |> 
    filter(Z == "0/0") |> # Same distribution in all Z-branches for estimation of w
    group_by(group) |> 
    summarise(expr = paste0("(", expr, ")", collapse = "+"),
              .groups = "drop") |> 
    rowwise() |> 
    mutate(caracas_symbol = list(as_sym(expr) |> simplify())) |> 
    ungroup()
  
  # n1
  #f1 <- d_tmp |> filter(group == "diag") |> pull(caracas_symbol) |> magrittr::extract2(1L) |> as_expr()
  f1 <- d_tmp |> filter(group == "diag") |> pull(caracas_symbol) |> magrittr::extract2(1L) |> simplify() |> as_expr()
  f1
  
  # n2
  f2 <- d_tmp |> filter(group == "corners") |> pull(caracas_symbol) |> magrittr::extract2(1L) |> as_expr()
  
  # n3
  f3 <- d_tmp |> filter(group == "off-diag") |> pull(caracas_symbol) |> magrittr::extract2(1L) |> as_expr()
  
  cat(
"
prob1func <- function(w) ", as.character(f1), "
prob2func <- function(w) ", as.character(f2), "
prob3func <- function(w) ", as.character(f3), "
", sep = "")
  
  # 
  # ##############
  # library(caracas)
  # def_sym(w)
  # t1 <- w^4 + 4 * w^2 * (w - 1)^2 + (w - 1)^4
  # t2 <- 2 * w^2 * (w - 1)^2
  # t3 <- 4 * w * (w^2 * (1 - w) - (w - 1)^3)
  # def_sym(n1, n2, n3)
  # f <- -(n1*log(t1) + n2*log(t2) + n3*log(t3))
  # f
  # f. <- der(f, w)
  # #f. <- "(4 * n1 * (w * (-6 * w^2 + 9 * w - 5) + 1))/(2 * (w - 1) * w * (3 * (w - 1) * w + 2) + 1) + (2 * n2 + n3)/(1 - w) - (2 * n2 + n3)/w + (n3 * (2 - 4 * w))/(2 * (w - 1) * w + 1)" |> 
  # #  as_sym()
  # 
  # #ws <- seq(1e-4, 0.1, length.out = 1000)
  # ws <- seq(0.01, 0.6, length.out = 1000)
  # f_val <- f |> as_expr() |> eval(list(n1 = 1000, n2 = 50, n3 = 10, w = ws))
  # f._val <- f. |> as_expr() |> eval(list(n1 = 1000, n2 = 50, n3 = 10, w = ws))
  # estimate_w(c(1000, 50, 10))
  # 
  # par(mfrow = c(1, 2))
  # plot(f_val ~ ws, type = "l")
  # plot(f._val ~ ws, type = "l"); abline(h = 0)
  # par(mfrow = c(1, 1))
  # 
  # solve_sys(f., w)
  # #caracas::eval_to_symbol("solve([w >= 0.5, w <= 3, w**2 - 1], w)")
  # caracas::eval_to_symbol("solve([w > 0, w < 1/2, 4*n1*(w*(-6*w^2 + 9*w - 5) + 1)/(w*(2*w - 2)*(w*(3*w - 3) + 2) + 1) + n3*(2 - 4*w)/(w*(2*w - 2) + 1) + (2*n2 + n3)/(1 - w) - (2*n2 + n3)/w], w)")
  # 
  # #f.. <- der(f., w)
  # f..
  # f.. |> as_expr()
  # 
  # as.character(f.)
  # 
  # 
  # ##############
  # library(Ryacas)
  # w <- ysym("w")
  # t1 <- w^4 + 4 * w^2 * (w - 1)^2 + (w - 1)^4
  # t2 <- 2 * w^2 * (w - 1)^2
  # t3 <- 4 * w * (w^2 * (1 - w) - (w - 1)^3)
  # n1 <- ysym("n1")
  # n2 <- ysym("n2")
  # n3 <- ysym("n3")
  # 
  # f <- -(n1*log(t1) + n2*log(t2) + n3*log(t3))
  # f. <- yac_str(paste0("D(w) ", f))
  # 
  # #ws <- seq(1e-4, 0.1, length.out = 1000)
  # ws <- seq(0.01, 0.6, length.out = 1000)
  # f_val <- f |> as_r() |> eval(list(n1 = 1000, n2 = 50, n3 = 10, w = ws))
  # f._val <- f. |> as_r() |> eval(list(n1 = 1000, n2 = 50, n3 = 10, w = ws))
  # 
  # par(mfrow = c(1, 2))
  # plot(f_val ~ ws, type = "l")
  # plot(f._val ~ ws, type = "l"); abline(h = 0)
  # par(mfrow = c(1, 1))
  # 
  # 
  # f.2 <- f. |> ysym() |> Ryacas::simplify()
  # f.2
  # #yac_str(paste0("Solve( ", f., ", w)"))
  # yac_str(paste0("Solve( ", f.2, ", w)"))
  # 
  # f.2num <- f.2 |> y_fn("Numer")
  # yac_str(paste0("Solve( ", f.2num, ", w)"))
}

prob1func <- function(w) w^4 + 4 * w^2 * (w - 1)^2 + (w - 1)^4
prob2func <- function(w) 2 * w^2 * (w - 1)^2
prob3func <- function(w) 4 * w * (w^2 * (1 - w) - (w - 1)^3)

#' Get counts from table
#' 
#' @param x Table
#' 
#' @export
get_ns <- function(x) {
  n1 <- 0L
  n2 <- 0L
  n3 <- 0L
  
  if (length(x) == 3L && all(x >= 0L)) {
    stopifnot(max(abs(x - round(x))) < 1e-12)
    n1 <- x[1L]
    n2 <- x[2L]
    n3 <- x[3L]
  } else {
    check_tab(x)
    
    n1 <- sum(diag(x))
    n2 <- x[1L, 3L] + x[3L, 1L]
    n3 <- sum(x) - n1 - n2
  }
  
  return(c(n1, n2, n3))
}

generate_mll <- function(x) {
  ns <- get_ns(x)
  n1 <- ns[1L]
  n2 <- ns[2L]
  n3 <- ns[3L]
  
  mll <- function(w) {
    -(n1*log(prob1func(w)) + n2*log(prob2func(w)) + n3*log(prob3func(w)))
  }
  
  mll
}

#' Estimate error probability, w
#' 
#' Note that `tol = .Machine$double.eps` unless overwritten.
#' 
#' @param x either a 3x3 contingency table or 
#'          3-vector with counts (`n1`, `n2`, `n3`) 
#'          where `n1 = sum(diag(x))`, `n2 = x[1L, 3L] + x[3L, 1L]`, 
#'          `n3 = sum(x) - n1 - n2` 
#' @param use_mpfr whether to use [Rmpfr::optimizeR()]] for arbitrary precision
#' @param \dots passed on to `optimise()` or to `Rmpfr::optimizeR` if `use_mpfr = TRUE`
#' 
#' @examples
#' tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
#' tab1
#' estimate_w(tab1)
#' n1 <- sum(diag(tab1))
#' n2 <- tab1[1L, 3L] + tab1[3L, 1L]
#' n3 <- sum(tab1) - n1 - n2
#' estimate_w(c(n1, n2, n3))
#' 
#' tab2 <- structure(c(40000L, 30L, 5L, 15L, 400L, 2L, 5L, 5L, 350L),
#'         dim = c(3L, 3L), class = "table")
#' tab2
#' estimate_w(tab2)
#' estimate_w(tab2, use_mpfr = TRUE)
#' 
#' @importFrom stats optimise
#' 
#' @export
estimate_w <- function(x, use_mpfr = FALSE, ...) {
  mll <- generate_mll(x)
  
  w_hat <- if (length(use_mpfr) == 1L && 
               is.logical(use_mpfr) && 
               use_mpfr == TRUE && 
               requireNamespace("Rmpfr", quietly = TRUE)) {
    Rmpfr::optimizeR(mll, lower = 1e-32, upper = .5, ...)$minimum
  } else {
    if ("tol" %in% names(list(...))) {
      stats::optimise(mll, interval = c(1e-32, .5), ...)$minimum
    } else {
      stats::optimise(mll, interval = c(1e-32, .5), tol = .Machine$double.eps, ...)$minimum
    }
  }
   
  w_hat
}

#' Estimate standard error of error probability, w
#' 
#' Note that this relies on the Hessian approximation returned by
#' [numDeriv::hessian()].
#' 
#' @param x Table, see [estimate_w()]
#' @param w estimated error probability w
#' @param method.args see [numDeriv::grad()]
#' 
#' @examples
#' x <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
#' x
#' w <- estimate_w(x)
#' w
#' estimate_w_se(x, w)
#' 
#' @export
estimate_w_se <- function(x, w, method.args = list()) {
  #mll <- generate_mll(x)
  #H <- optimHess(par = w, fn = mll, control = control)
  #H <- numDeriv::hessian(mll, w, method = "Richardson", method.args = method.args)
  #sqrt(diag(solve(H)))
  
  ns <- get_ns(x)
  n1 <- ns[1L]
  n2 <- ns[2L]
  n3 <- ns[3L]
  
  H <- 4 * n1 * (w * (-6 * w ^ 2 + 9 * w - 5) + 1) * (-w *
                                                        (2 * w - 2) * (6 * w - 3) - 2 * w * (w * (3 * w - 3) + 2) -
                                                        (2 * w - 2) * (w * (3 * w - 3) + 2)) /
    (w * (2 * w - 2) * (w *
                          (3 * w - 3) + 2) + 1) ^
    2 + 4 * n1 * (-6 * w ^ 2 + w * (9 - 12 *
                                      w) + 9 * w - 5) /
    (w * (2 * w - 2) * (w * (3 * w - 3) + 2) +
       1) + n3 * (2 - 4 * w) ^
    2 / (w * (2 * w - 2) + 1) ^ 2 - 4 * n3 / (w *
                                                (2 * w - 2) + 1) + (2 * n2 + n3)/(1 - w)^2 + (2 * n2 + n3)/w^2
  
  if (is.na(H) || is.infinite(H) || H < 0) {
    return(NA_real_)
  }
  
  return(sqrt(1/H))
}


#' Estimate error probability, w, using a Bayesian approach
#' 
#' A Beta prior is assumed. Note that $w$ is assumed to be $< 0.5$.
#' 
#' NOTE: We recommend using e.g. the `cmdstanr`, see 
#' the vignette. This is just an implementation that demonstrate the 
#' Bayesian approach.
#' 
#' @param x either a 3x3 contingency table or 
#'          3-vector with counts (`n1`, `n2`, `n3`) 
#'          where `n1 = sum(diag(x))`, `n2 = x[1L, 3L] + x[3L, 1L]`, 
#'          `n3 = sum(x) - n1 - n2` 
#' @param prior_a First shape parameter for prior beta distribution
#' @param prior_b Second shape parameter for prior beta distribution
#' @param chains Number of chains to run
#' @param chains_function Function to run the chains (fx `lapply` or `parallel::mclapply`)
#' @param warmup Number of warm-up iterations
#' @param iterations Number of iterations (after warm-up)
#' @param \dots Not used
#' 
#' @examples
#' tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
#' tab1
#' estimate_w(tab1)
#' y <- estimate_w_bayesian(tab1)
#' q <- posterior_samples(y)
#' mean(q); #plot(q, type = "l"); hist(q)
#' sapply(y, \(x) x$acceptance_ratio)
#' 
#' estimate_w_bayesian(c(750, 0, 0), chains = 1, warmup = 0, iterations = 500) |> 
#'    lapply(\(x) x$samples) |> unlist() |> plot(type = "l")
#' 
#' estimate_w(c(750, 0, 0))
#' y <- estimate_w_bayesian(c(750, 0, 0)); q <- lapply(y, \(x) x$samples) |> unlist()
#' mean(q); #plot(q, type = "l"); hist(q)
#' sapply(y, \(x) x$acceptance_ratio)
#' 
#' @importFrom stats runif
#' 
#' @export
estimate_w_bayesian <- function(x, 
                                prior_a = 1, prior_b = 1, 
                                chains = 4,
                                chains_function = lapply,
                                warmup = 1000, iterations = 10000, ...) {
  
  mll <- generate_mll(x)
  
  proposal <- function(old_w) {
    lwr <- max(0, old_w - 0.01)
    upr <- min(0.5, old_w + 0.01)
    stats::runif(1, lwr, upr)
  }
  
  log_posterior <- function(w) {
    # mll is negative log likelihood (minimised in other functions)
    -mll(w) + dbeta05(w, shape1 = prior_a, shape2 = prior_b, log = TRUE)
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
    
    return(list(samples = samples, acceptance_ratio = acceptances / n))
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

#' Extract posterior samples
#' 
#' @param x Result from [estimate_w_bayesian()]
#' 
#' @examples
#' tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
#' tab1
#' y <- estimate_w_bayesian(tab1)
#' q <- posterior_samples(y)
#' mean(q); #plot(q, type = "l"); hist(q)
#' 
#' @importFrom methods is
#' 
#' @export
posterior_samples <- function(x) {
  stopifnot(methods::is(x, "wgsLR_bayesian_sample"))
  lapply(x, \(y) y$samples) |> unlist()
}


#' Extract posterior samples as data.frame
#' 
#' @param x Result from [estimate_w_bayesian()]
#' 
#' @examples
#' tab1 <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
#' tab1
#' y <- estimate_w_bayesian(tab1)
#' q <- posterior_samples_df(y)
#' head(q)
#' mean(q$sample); #ggplot(q, aes(iteration, sample, colour = factor(chain))) + geom_line()
#' 
#' @importFrom methods is
#' 
#' @export
posterior_samples_df <- function(x) {
  stopifnot(methods::is(x, "wgsLR_bayesian_sample"))
  do.call(rbind, lapply(seq_along(x), \(i) data.frame(chain = i, iteration = seq_along(x[[i]]$samples), sample = x[[i]]$samples)))
}





#' Log-likelihood of w from a single sample assuming Hardy-Weinberg equilibrium
#' 
#' @param x Number of alternative alleles (0/1/2)
#' @param p list of genotype probabilities or vector of length 3 for single locus
#' @param w single number, genotyping error probability
logliks_single_sample <- function(x, p, w) {
  
  
  if (FALSE) {
    library(tidyverse)
    library(caracas)
    
    d_tmp <- d_formulas_single |> 
      mutate(p = case_when(
        Z_MA == 0L ~ "p0",
        Z_MA == 1L ~ "p1",
        Z_MA == 2L ~ "p2"
      )) |> 
      # p_1 is for both 0/1 and 1/0, so only take half for those cases
      mutate(weight = ifelse(Z_MA == 1L, 2, 1)) |> 
      mutate(expr = paste0("(", p, "/", weight, ") * ", expr)) |>
      group_by(X_MA) |> 
      summarise(expr = paste0(expr, collapse = " + "), 
                .groups = "drop") 
    d_tmp
    
    x <- d_tmp |> pull(expr) |> as_sym() |> simplify() 
    x
    z <- x |> 
      subs(nms = c("p0", "p1", "p2", "w"), vls = c("(1-0.25)^2", "2*0.25*(1-0.25)", "0.25^2"))
    z
    z |> sum()
    
    x
    x[1]
    xlog <- x |> log() |> simplify()
    as_expr(xlog[1])
    as_expr(xlog[2])
    as_expr(xlog[3])
    
  }
  
  n <- length(x)
  p <- reuse_genotype_probs(p = p, n = n)
  check_p(p)
  stopifnot(is.integer(x))
  stopifnot(all(x == 0L | x == 1L | x == 2L))
  stopifnot(length(w) == 1L)
  stopifnot(w >= 0 && w <= 1)
  a <- (w - 1)^2
  b <- w * (w - 1)
  c <- w^2
  
  logliks <- vapply(seq_len(n), function(j) {
    p0 <- p[[j]][1]
    p1 <- p[[j]][2]
    p2 <- p[[j]][3]
    
    val <- switch(
      x[j] + 1,
      
      # x[j] == 0L
      p0 * a - p1 * b + p2 * c,
      
      # x[j] == 1L
      -2 * p0 * b + p1 * (a + c) - 2 * p2 * b,
      
      # x[j] == 2L
      p0 * c - p1 * b + p2 * a
    )
    
    log(val)
  }, numeric(1))
  
  
  if (FALSE) {
    # For fixed w, it must sum to 1:
    q <- 0.25
    y <- logliks_single_sample(x = 0L:2L, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1)
    y
    exp(y)
    exp(y) |> sum()
    y <- logliks_single_sample_old(x = 0L:2L, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1)
    y
    exp(y)
    exp(y) |> sum()
    
    
    logliks_single_sample_Rcpp <- function(x, p, w) {
      n <- length(x)
      p <- reuse_genotype_probs(p = p, n = n)
      check_p(p)
      stopifnot(is.integer(x))
      stopifnot(all(x == 0L | x == 1L | x == 2L))
      stopifnot(length(w) == 1L)
      stopifnot(w >= 0 && w <= 1)
      
      logliks_single_sample_Rcpp_wrapper(x, p, w)
    }
    
    library(Rcpp)
    tmpfile <- paste0(tempfile(), ".cpp")
    cat(logliks_single_sample_Rcpp_code(), file = tmpfile)
    sourceCpp(tmpfile)
    
    y <- logliks_single_sample_Rcpp(x = 0L:2L, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1)
    y
    exp(y)
    exp(y) |> sum()
    
    
    q <- 0.25
    Z <- sample_profiles_without_error(n = 1000, p = c((1-q)^2, 2*q*(1-q), q^2))
    X <- add_errors_to_genotypes(Z, 0.25) |> to012() |> c()
    
    microbenchmark::microbenchmark(
      logliks_single_sample(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1),
      logliks_single_sample_old(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1),
      times = 10
    )
    
    microbenchmark::microbenchmark(
      logliks_single_sample(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1),
      logliks_single_sample_old(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1),
      logliks_single_sample_Rcpp(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = 0.1),
      times = 10
    )
    
    q1 <- 0.25
    q2 <- 0.99
    y <- lapply(0L:2L, \(xi) logliks_single_sample(x = c(xi, xi), p = list(
      c((1-q1)^2, 2*q1*(1-q1), q1^2),
      c((1-q2)^2, 2*q2*(1-q2), q2^2)
    ), w = 0.1))
    y <- purrr::transpose(y) |> lapply(unlist)
    y
    lapply(y, exp)
    lapply(y, exp) |> lapply(sum)
    
    # For multiple samples, it must be maximal at w
    q <- 0.25
    Z <- sample_profiles_without_error(n = 1000, p = c((1-q)^2, 2*q*(1-q), q^2))
    X <- add_errors_to_genotypes(Z, 0.25) |> to012() |> c()

    #ws <- seq(0, 1, length.out = 101)
    ws <- seq(0.2, 0.3, length.out = 101)
    library(parallel)
    options(mc.cores = 8)
    logliks <- mclapply(ws, \(w) {
      #logliks_single_sample(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = w) |> sum()
      logliks_single_sample_Rcpp(x = X, p = c((1-q)^2, 2*q*(1-q), q^2), w = w) |> sum()
    }) |> unlist()
    logliks
    plot(ws, logliks)
    abline(v = 0.25)
  }
  
  return(logliks)
}




logliks_single_sample_old <- function(x, p, w) {
  
  
  n <- length(x)
  p <- reuse_genotype_probs(p = p, n = n)
  check_p(p)
  stopifnot(is.integer(x))
  stopifnot(all(x == 0L | x == 1L | x == 2L))
  stopifnot(length(w) == 1L)
  stopifnot(w >= 0 && w <= 1)
  
  
  logliks <- lapply(seq_len(n), \(j) {
    pj <- p[[j]]
    p0 <- pj[1L]
    p1 <- pj[2L]
    p2 <- pj[3L]
    
    xj <- x[j]
    
    if (xj == 0L) {
      log(p0 * (w - 1)^2 - p1 * w * (w - 1) + p2 * w^2)
    } else if (xj == 1L) {
      log(-2 * p0 * w * (w - 1) + p1 * w^2 + p1 * (w - 1)^2 - 2 * p2 * w * (w - 1))
    } else {
      # xj == 2L
      log(p0 * w^2 - p1 * w * (w - 1) + p2 * (w - 1)^2)
    }
  }) |> unlist()
  
  
  return(logliks)
}



logliks_single_sample_Rcpp_code <- function() {
  paste0('
    #include <Rcpp.h>
    using namespace Rcpp;
    
    // [[Rcpp::export]]
    NumericVector logliks_single_sample_Rcpp_wrapper(IntegerVector x, List p, double w) {
      const int n = x.size();
      NumericVector out(n);
    
      const double a = (w - 1.0) * (w - 1.0);
      const double b = w * (w - 1.0);
      const double c = w * w;
    
      for (int j = 0; j < n; ++j) {
        NumericVector pj = p[j];
    
        const double p0 = pj[0];
        const double p1 = pj[1];
        const double p2 = pj[2];
    
        double val;
    
        switch (x[j]) {
        case 0:
          val = p0 * a - p1 * b + p2 * c;
          break;
        case 1:
          val = -2.0 * p0 * b +
                p1 * (a + c) -
                2.0 * p2 * b;
          break;
        default: // x[j] == 2
          val = p0 * c - p1 * b + p2 * a;
        }
    
        out[j] = std::log(val);
      }
    
      return out;
    }        
             
    ')
}

