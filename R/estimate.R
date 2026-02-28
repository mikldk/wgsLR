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


#' @rdname beta05
#' @export
rbeta05 <- function(n, shape1, shape2) {
  rbeta(n, shape1, shape2) / 2
}

#' @rdname beta05
#' @export
pbeta05 <- function(x, shape1, shape2) {
  pbeta(2 * x, shape1, shape2)
}

#' @rdname beta05
#' @export
qbeta05 <- function(x, shape1, shape2) {
  qbeta(x, shape1, shape2) / 2
}

#' Beta distribution on (0, 0.5)
#' 
#' @param x number between 0 and 0.5
#' @param n number of random samples to draw
#' @param shape1 first shape parameter
#' @param shape2 second shape parameter
#' @param log return result on log scale
#' 
#' @rdname beta05
#' 
#' @examples
#' dbeta05(0.2, 1, 5)
#' dbeta05(0.2, 1, 5, log = TRUE)
#' dbeta05(0.2, 1, 5) |> log()
#' 
#' pbeta05(0.5, 1, 5)
#' 
#' qbeta05(1, 1, 5)
#' 
#' #rbeta05(100, 1, 5) |> hist(probability = TRUE)
#' #curve(dbeta05(x, 1, 5), from = 0, to = 0.5, add = TRUE)
#' 
#' @export
dbeta05 <- function(x, shape1, shape2, log = FALSE) {
  if (log) {
    d <- log(2) + dbeta(2 * x, shape1, shape2, log = TRUE)
    d
    return(d)
  }
  
  d <- 2 * dbeta(2 * x, shape1, shape2, log = log)
  d
}


if (FALSE) {
  x <- rbeta05(1000, 5, 1)
  hist(x, probability = TRUE)
  curve(dbeta05(x, 5, 1), from = 0, to = 1, add = TRUE)
  integrate(dbeta05, lower = 0, upper = 0.5, shape1 = 5, shape2 = 1)
  integrate(dbeta05, lower = 0, upper = 1, shape1 = 5, shape2 = 1)
  
  integrate(dbeta05, lower = 0, upper = 0.25, shape1 = 5, shape2 = 1)
  pbeta05(0.25, 5, 1)
  qbeta05(0.03125, 5, 1)
  
  integrate(dbeta05, lower = 0, upper = 0.1, shape1 = 5, shape2 = 1)
  pbeta05(0.1, 5, 1)
  qbeta05(0.00032, 5, 1)
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
#' @param iterations Number of iterations
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
