if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wTwR()
  
  library(tidyverse)
  library(caracas)
  
  
  code <- ""
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      
      e <- d_prob_Hp_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      #e <- d_prob_Hd_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      
      condition <- paste0("else if (xT == ", xT, "L && xR == ", xR, "L) ")
      
      if (xT == 0L && xR == 0L) {
        condition <- paste0("if (xT == ", xT, "L && xR == ", xR, "L) ")
      } 
      code <- paste0(code, condition, " {
            return(", e, ")
          }\n")
    }
  }
  code <- paste0(code, " else {
    stop(\"Not recognised\")
  }")
  
  cat(code)
}

calc_LR_num_Hp_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2) {
  if (xT == 0L && xR == 0L)  {
    return(p_0*(wT - 1)^2*(wR - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wT^2*wR^2)
  }
  else if (xT == 0L && xR == 1L)  {
    return(-2*p_0*wR*(wT - 1)^2*(wR - 1) - p_1*wT*wR^2*(wT - 1) - p_1*wT*(wT - 1)*(wR - 1)^2 - 2*p_2*wT^2*wR*(wR - 1))
  }
  else if (xT == 0L && xR == 2L)  {
    return(p_0*wR^2*(wT - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wT^2*(wR - 1)^2)
  }
  else if (xT == 1L && xR == 0L)  {
    return(-2*p_0*wT*(wT - 1)*(wR - 1)^2 - p_1*wT^2*wR*(wR - 1) - p_1*wR*(wT - 1)^2*(wR - 1) - 2*p_2*wT*wR^2*(wT - 1))
  }
  else if (xT == 1L && xR == 1L)  {
    return(4*p_0*wT*wR*(wT - 1)*(wR - 1) + p_1*wT^2*wR^2 + p_1*wT^2*(wR - 1)^2 + p_1*wR^2*(wT - 1)^2 + p_1*(wT - 1)^2*(wR - 1)^2 + 4*p_2*wT*wR*(wT - 1)*(wR - 1))
  }
  else if (xT == 1L && xR == 2L)  {
    return(-2*p_0*wT*wR^2*(wT - 1) - p_1*wT^2*wR*(wR - 1) - p_1*wR*(wT - 1)^2*(wR - 1) - 2*p_2*wT*(wT - 1)*(wR - 1)^2)
  }
  else if (xT == 2L && xR == 0L)  {
    return(p_0*wT^2*(wR - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wR^2*(wT - 1)^2)
  }
  else if (xT == 2L && xR == 1L)  {
    return(-2*p_0*wT^2*wR*(wR - 1) - p_1*wT*wR^2*(wT - 1) - p_1*wT*(wT - 1)*(wR - 1)^2 - 2*p_2*wR*(wT - 1)^2*(wR - 1))
  }
  else if (xT == 2L && xR == 2L)  {
    return(p_0*wT^2*wR^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*(wT - 1)^2*(wR - 1)^2)
  }
  else {
    stop("Not recognised")
  }
}


calc_LR_den_Hd_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2) {
  if (xT == 0L && xR == 0L)  {
    return(p_0^2*(wT - 1)^2*(wR - 1)^2 - p_0*p_1*wT*(wT - 1)*(wR - 1)^2 - p_0*p_1*wR*(wT - 1)^2*(wR - 1) + p_0*p_2*wT^2*(wR - 1)^2 + p_0*p_2*wR^2*(wT - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT^2*wR*(wR - 1) - p_1*p_2*wT*wR^2*(wT - 1) + p_2^2*wT^2*wR^2)
  }
  else if (xT == 0L && xR == 1L)  {
    return(-2*p_0^2*wR*(wT - 1)^2*(wR - 1) + 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) + p_0*p_1*wR^2*(wT - 1)^2 + p_0*p_1*(wT - 1)^2*(wR - 1)^2 - 2*p_0*p_2*wT^2*wR*(wR - 1) - 2*p_0*p_2*wR*(wT - 1)^2*(wR - 1) - p_1^2*wT*wR^2*(wT - 1) - p_1^2*wT*(wT - 1)*(wR - 1)^2 + p_1*p_2*wT^2*wR^2 + p_1*p_2*wT^2*(wR - 1)^2 + 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) - 2*p_2^2*wT^2*wR*(wR - 1))
  }
  else if (xT == 0L && xR == 2L)  {
    return(p_0^2*wR^2*(wT - 1)^2 - p_0*p_1*wT*wR^2*(wT - 1) - p_0*p_1*wR*(wT - 1)^2*(wR - 1) + p_0*p_2*wT^2*wR^2 + p_0*p_2*(wT - 1)^2*(wR - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT^2*wR*(wR - 1) - p_1*p_2*wT*(wT - 1)*(wR - 1)^2 + p_2^2*wT^2*(wR - 1)^2)
  }
  else if (xT == 1L && xR == 0L)  {
    return(-2*p_0^2*wT*(wT - 1)*(wR - 1)^2 + p_0*p_1*wT^2*(wR - 1)^2 + 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) + p_0*p_1*(wT - 1)^2*(wR - 1)^2 - 2*p_0*p_2*wT*wR^2*(wT - 1) - 2*p_0*p_2*wT*(wT - 1)*(wR - 1)^2 - p_1^2*wT^2*wR*(wR - 1) - p_1^2*wR*(wT - 1)^2*(wR - 1) + p_1*p_2*wT^2*wR^2 + 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) + p_1*p_2*wR^2*(wT - 1)^2 - 2*p_2^2*wT*wR^2*(wT - 1))
  }
  else if (xT == 1L && xR == 1L)  {
    return(4*p_0^2*wT*wR*(wT - 1)*(wR - 1) - 2*p_0*p_1*wT^2*wR*(wR - 1) - 2*p_0*p_1*wT*wR^2*(wT - 1) - 2*p_0*p_1*wT*(wT - 1)*(wR - 1)^2 - 2*p_0*p_1*wR*(wT - 1)^2*(wR - 1) + 8*p_0*p_2*wT*wR*(wT - 1)*(wR - 1) + p_1^2*wT^2*wR^2 + p_1^2*wT^2*(wR - 1)^2 + p_1^2*wR^2*(wT - 1)^2 + p_1^2*(wT - 1)^2*(wR - 1)^2 - 2*p_1*p_2*wT^2*wR*(wR - 1) - 2*p_1*p_2*wT*wR^2*(wT - 1) - 2*p_1*p_2*wT*(wT - 1)*(wR - 1)^2 - 2*p_1*p_2*wR*(wT - 1)^2*(wR - 1) + 4*p_2^2*wT*wR*(wT - 1)*(wR - 1))
  }
  else if (xT == 1L && xR == 2L)  {
    return(-2*p_0^2*wT*wR^2*(wT - 1) + p_0*p_1*wT^2*wR^2 + 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) + p_0*p_1*wR^2*(wT - 1)^2 - 2*p_0*p_2*wT*wR^2*(wT - 1) - 2*p_0*p_2*wT*(wT - 1)*(wR - 1)^2 - p_1^2*wT^2*wR*(wR - 1) - p_1^2*wR*(wT - 1)^2*(wR - 1) + p_1*p_2*wT^2*(wR - 1)^2 + 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) + p_1*p_2*(wT - 1)^2*(wR - 1)^2 - 2*p_2^2*wT*(wT - 1)*(wR - 1)^2)
  }
  else if (xT == 2L && xR == 0L)  {
    return(p_0^2*wT^2*(wR - 1)^2 - p_0*p_1*wT^2*wR*(wR - 1) - p_0*p_1*wT*(wT - 1)*(wR - 1)^2 + p_0*p_2*wT^2*wR^2 + p_0*p_2*(wT - 1)^2*(wR - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT*wR^2*(wT - 1) - p_1*p_2*wR*(wT - 1)^2*(wR - 1) + p_2^2*wR^2*(wT - 1)^2)
  }
  else if (xT == 2L && xR == 1L)  {
    return(-2*p_0^2*wT^2*wR*(wR - 1) + p_0*p_1*wT^2*wR^2 + p_0*p_1*wT^2*(wR - 1)^2 + 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) - 2*p_0*p_2*wT^2*wR*(wR - 1) - 2*p_0*p_2*wR*(wT - 1)^2*(wR - 1) - p_1^2*wT*wR^2*(wT - 1) - p_1^2*wT*(wT - 1)*(wR - 1)^2 + 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) + p_1*p_2*wR^2*(wT - 1)^2 + p_1*p_2*(wT - 1)^2*(wR - 1)^2 - 2*p_2^2*wR*(wT - 1)^2*(wR - 1))
  }
  else if (xT == 2L && xR == 2L)  {
    return(p_0^2*wT^2*wR^2 - p_0*p_1*wT^2*wR*(wR - 1) - p_0*p_1*wT*wR^2*(wT - 1) + p_0*p_2*wT^2*(wR - 1)^2 + p_0*p_2*wR^2*(wT - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT*(wT - 1)*(wR - 1)^2 - p_1*p_2*wR*(wT - 1)^2*(wR - 1) + p_2^2*(wT - 1)^2*(wR - 1)^2)
  }
  else {
    stop("Not recognised")
  }
}







################################################################################


int_LR_num_Hp_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2, shape1D, shape2D) {
  if (xT == 0L && xR == 0L)  {
    return(int_Hp_xT0_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 0L && xR == 1L)  {
    return(int_Hp_xT0_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 0L && xR == 2L)  {
    return(int_Hp_xT0_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 0L)  {
    return(int_Hp_xT1_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 1L)  {
    return(int_Hp_xT1_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 2L)  {
    return(int_Hp_xT1_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 0L)  {
    return(int_Hp_xT2_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 1L)  {
    return(int_Hp_xT2_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 2L)  {
    return(int_Hp_xT2_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else {
    stop("Not recognised")
  }
}


int_LR_den_Hd_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2, shape1D, shape2D) {
  if (xT == 0L && xR == 0L)  {
    return(int_Hd_xT0_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 0L && xR == 1L)  {
    return(int_Hd_xT0_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 0L && xR == 2L)  {
    return(int_Hd_xT0_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 0L)  {
    return(int_Hd_xT1_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 1L)  {
    return(int_Hd_xT1_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 1L && xR == 2L)  {
    return(int_Hd_xT1_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 0L)  {
    return(int_Hd_xT2_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 1L)  {
    return(int_Hd_xT2_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xT == 2L && xR == 2L)  {
    return(int_Hd_xT2_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else {
    stop("Not recognised")
  }
}







#' Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using Monte Carlo integration
#' 
#' @examples
#' calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' shpS <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-7, a = 0, b = 0.5)
#' integrate(function(x) dbeta05(x, shape1 = shpS[1L], shape2 = shpS[2L]), lower = 1e-12, upper = 0.5, rel.tol = 1e-6)
#' calc_LRs_wTwR_integrate_wTwR_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   shape1S = shpS[1], shape2S = shpS[2], 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, shpS[1], shpS[2]), from = 0, to = 0.5)
#' 
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param shape1T_H1 Under $H_1$ (in $LR$'s numerator), `wT` has beta prior on (0, 0.5) with parameters `shape1T_H1` and `shape2T_H1`
#' @param shape2T_H1 see `shape1T_Hp`
#' @param shape1T_H2 Under $H_2$ (in $LR$'s denominator), `wT` has beta prior (on 0-0.5) with parameters `shape1T_H2` and `shape2T_H2`
#' @param shape2T_H2 see `shape1T_H2`
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' @param n_samples number of random samples from each prior distribution
#'
#' @export
calc_LRs_wTwR_integrate_wT_mc <- function(xT, xR, shape1T_H1, shape2T_H1, shape1T_H2, shape2T_H2, wR, p, n_samples = 1000) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  wTs_H1 <- rbeta05(n_samples, shape1 = shape1T_H1, shape2 = shape2T_H1)
  wTs_H2 <- rbeta05(n_samples, shape1 = shape1T_H2, shape2 = shape2T_H2)
  
  LR_mc <- lapply(seq_len(n_samples), \(i_mc) {
    wT_H1_i_mc <- wTs_H1[i_mc]
    wT_H2_i_mc <- wTs_H2[i_mc]
    
    LRs_i_mc <- unlist(lapply(seq_along(xR), function(i) {
      pi <- p[[i]]
      
      LR_num <- calc_LR_num_Hp_single_no_checks_wTwR(
        xR = xR[i], xT = xT[i], 
        wT = wT_H1_i_mc, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      LR_den <- calc_LR_den_Hd_single_no_checks_wTwR(
        xR = xR[i], xT = xT[i], 
        wT = wT_H2_i_mc, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      LR_num / LR_den
    }))
    
    LRs_i_mc
  })
  
  LRs <- unlist(lapply(seq_along(xR), function(i) {
    mean(unlist(lapply(seq_along(LR_mc), \(j) LR_mc[[j]][i])))
  }))
  
  return(LRs)
}

#' Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using numerical integration
#' 
#' NOTE: Unless you have a reason, please use [calc_LRs_wTwR_integrate_wT()] instead.
#' 
#' @examples
#' calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wTwR_integrate_wT_num(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' 
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wTwR_integrate_wT_num(
#'   xT = c(0, 0),
#'   xR = c(0, 1),
#'   shape1D = 1, shape2D = 1,
#'   wR = 1e-5,
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param shape1T_H1 Under $H_1$ (in $LR$'s numerator), `wT` has beta prior on (0, 0.5) with parameters `shape1T_H1` and `shape2T_H1`
#' @param shape2T_H1 see `shape1T_Hp`
#' @param shape1T_H2 Under $H_2$ (in $LR$'s denominator), `wT` has beta prior (on 0-0.5) with parameters `shape1T_H2` and `shape2T_H2`
#' @param shape2T_H2 see `shape1T_H2`
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' @param lower_int lowest value to integrate from; should be 0, but numerical instability can make this problematic
#' @param \dots pass on to [integrate()], e.g., `rel.tol` and `abs.tol`
#'
#' @export
calc_LRs_wTwR_integrate_wT_num <- function(xT, xR, shape1T_H1, shape2T_H1, shape1T_H2, shape2T_H2, wR, p, lower_int = 0, ...) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  LRs <- lapply(seq_along(p), \(i) {
    xTi <- xT[i]
    xRi <- xR[i]
    pi <- p[[i]]
    
    f_num <- function(wT) {
      LR_num <- calc_LR_num_Hp_single_no_checks_wTwR(xT = xTi, 
                                                     xR = xRi,
                                                     wT = wT, 
                                                     wR = wR,
                                                     p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      z <- dbeta05(wT, shape1 = shape1T_H1, shape2 = shape2T_H1, log = TRUE) + log(LR_num)
      exp(z)
    }
    
    f_den <- function(wT) {
      LR_den <- calc_LR_den_Hd_single_no_checks_wTwR(xT = xTi, 
                                                     xR = xRi,
                                                     wT = wT, 
                                                     wR = wR,
                                                     p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      z <- dbeta05(wT, shape1 = shape1T_H2, shape2 = shape2T_H2, log = TRUE) + log(LR_den)
      exp(z)
    }
    
    LR_num <- integrate(f_num, lower = lower_int, upper = 0.5, ...)
    LR_den <- integrate(f_den, lower = lower_int, upper = 0.5, ...)
    
    LR_num$value / LR_den$value
  })

  LRs <- unlist(LRs)
  
  return(LRs)
}

#' Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using exact integration
#' 
#' NOTE: Unless you have a reason, please use [calc_LRs_wTwR_integrate_wT()] instead.
#' 
#' @examples
#' calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wTwR_integrate_wT_num(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' 
#' calc_LRs_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wTwR_integrate_wT_num(
#'   xT = c(0, 0),
#'   xR = c(0, 1),
#'   shape1D = 1, shape2D = 1,
#'   wR = 1e-5,
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param shape1T_H1 Under $H_1$ (in $LR$'s numerator), `wT` has beta prior on (0, 0.5) with parameters `shape1T_H1` and `shape2T_H1`
#' @param shape2T_H1 see `shape1T_Hp`
#' @param shape1T_H2 Under $H_2$ (in $LR$'s denominator), `wT` has beta prior (on 0-0.5) with parameters `shape1T_H2` and `shape2T_H2`
#' @param shape2T_H2 see `shape1T_H2`
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' @param use_mpfr use higher precision numbers via the `Rmpfr` package
#' @param stop_on_infinite stop if infinite numbers are encountered (if so, try `use_mpfr = TRUE`)
#' @param mpfr_precision number of bits to use
#' 
#' @importFrom Rmpfr beta
#' 
#' @export
calc_LRs_wTwR_integrate_wT <- function(xT, xR, shape1T_H1, shape2T_H1, shape1T_H2, shape2T_H2, wR, p, use_mpfr = TRUE, stop_on_infinite = TRUE, mpfr_precision = 256) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  if (use_mpfr) {
    if (!inherits(shape1T_H1, "mpfr")) {
      shape1T_H1 <- mpfr(shape1T_H1, precBits = mpfr_precision)
    }
    if (!inherits(shape2T_H1, "mpfr")) {
      shape2T_H1 <- mpfr(shape2T_H1, precBits = mpfr_precision)
    }
    
    
    if (!inherits(shape1T_H2, "mpfr")) {
      shape1T_H2 <- mpfr(shape1T_H2, precBits = mpfr_precision)
    }
    if (!inherits(shape2T_H2, "mpfr")) {
      shape2T_H2 <- mpfr(shape2T_H2, precBits = mpfr_precision)
    }
    
    
    if (!inherits(wR, "mpfr")) {
      wR <- mpfr(wR, precBits = mpfr_precision)
    }
  }
  
  LRs <- lapply(seq_along(p), \(i) {
    xTi <- xT[i]
    xRi <- xR[i]
    p_i <- p[[i]]
    
    if (use_mpfr) {
      if (!inherits(p_i, "mpfr")) {
        p_i <- mpfr(p_i, precBits = mpfr_precision)
      }
    }
    
    LR_num <- int_LR_num_Hp_single_no_checks_wTwR(xT = xTi, xR = xRi, wR = wR, p_0 = p_i[1L], p_1 = p_i[2L], p_2 = p_i[3L], shape1D = shape1T_H1, shape2D = shape2T_H1)
    LR_den <- int_LR_den_Hd_single_no_checks_wTwR(xT = xTi, xR = xRi, wR = wR, p_0 = p_i[1L], p_1 = p_i[2L], p_2 = p_i[3L], shape1D = shape1T_H2, shape2D = shape2T_H2)
    
    LR <- LR_num/LR_den
    
    if (use_mpfr) {
      LR <- as.numeric(LR_num/LR_den)
    }
    
    LR
  })
  LRs <- unlist(LRs)
  
  if (stop_on_infinite && any(!is.finite(LRs))) {
    stop("Numeric problems, consider calling with argument use_mpfr = TRUE")
  }
  
  return(LRs)
}

