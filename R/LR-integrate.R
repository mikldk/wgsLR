if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wTwR()
  
  library(tidyverse)
  library(caracas)
  
  
  code <- ""
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      
      e <- d_prob_Hp_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      #e <- d_prob_Ha_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      
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


calc_LR_den_Ha_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2) {
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


int_LR_num_Hp_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2, shape1T, shape2T) {
  if (xT == 0L && xR == 0L)  {
    return(int_Hp_xT0_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 0L && xR == 1L)  {
    return(int_Hp_xT0_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 0L && xR == 2L)  {
    return(int_Hp_xT0_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 0L)  {
    return(int_Hp_xT1_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 1L)  {
    return(int_Hp_xT1_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 2L)  {
    return(int_Hp_xT1_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 0L)  {
    return(int_Hp_xT2_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 1L)  {
    return(int_Hp_xT2_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 2L)  {
    return(int_Hp_xT2_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else {
    stop("Not recognised")
  }
}


int_LR_den_Ha_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2, shape1T, shape2T) {
  if (xT == 0L && xR == 0L)  {
    return(int_Ha_xT0_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 0L && xR == 1L)  {
    return(int_Ha_xT0_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 0L && xR == 2L)  {
    return(int_Ha_xT0_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 0L)  {
    return(int_Ha_xT1_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 1L)  {
    return(int_Ha_xT1_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 1L && xR == 2L)  {
    return(int_Ha_xT1_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 0L)  {
    return(int_Ha_xT2_xR0(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 1L)  {
    return(int_Ha_xT2_xR1(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else if (xT == 2L && xR == 2L)  {
    return(int_Ha_xT2_xR2(wR = wR, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1T, b = shape2T))
  }
  else {
    stop("Not recognised")
  }
}







#' Calculate WoE for a profile for sample-specific error probabilities integrated over the donor prior distribution using Monte Carlo integration
#' 
#' @examples
#' calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5)) |> log10() |> sum()
#' 
#' shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-7, a = 0, b = 0.5)
#' # curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
#' z1 <- calc_WoE_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1T_Hp = shpT[1], 
#'   shape2T_Hp = shpT[2],
#'   shape1T_Ha = shpT[1], 
#'   shape2T_Ha = shpT[2],
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 1000)
#' z1
#' 
#' z2 <- calc_WoE_wTwR_integrate_wT_num(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1T_Hp = shpT[1], 
#'   shape2T_Hp = shpT[2],
#'   shape1T_Ha = shpT[1], 
#'   shape2T_Ha = shpT[2],
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' z2
#'   
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param shape1T_Hp under $H_p$ `wT` has beta prior on (0, 0.5) with parameters `shape1T_Hp` and `shape2T_Hp`
#' @param shape2T_Hp see `shape1T_Hp`
#' @param shape1T_Ha under $H_a$ `wT` has beta prior on (0, 0.5) with parameters `shape1T_Ha` and `shape2T_Ha`
#' @param shape2T_Ha see `shape1T_Ha`
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' @param n_samples number of random samples from each prior distribution
#'
#' @export
calc_WoE_wTwR_integrate_wT_mc <- function(xT, xR, shape1T_Hp, shape2T_Hp, shape1T_Ha, shape2T_Ha, wR, p, n_samples = 1000) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  wTs_Hp <- rbeta05(n_samples, shape1 = shape1T_Hp, shape2 = shape2T_Hp)
  LR_Hp_mc <- unlist(lapply(wTs_Hp, \(wT) {
    fcts_LR <- unlist(lapply(seq_along(xR), function(i) {
      pi <- p[[i]]
      xTi <- xT[i]
      xRi <- xR[i]
      
      calc_LR_num_Hp_single_no_checks_wTwR(
        xT = xTi, xR = xRi, 
        wT = wT, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
    }))
    
    prod(fcts_LR)
  })) |> mean()
  rm(wTs_Hp) # Avoid accidental re-use
  
  wTs_Ha <- rbeta05(n_samples, shape1 = shape1T_Ha, shape2 = shape2T_Ha)
  LR_Ha_mc <- unlist(lapply(wTs_Ha, \(wT) {
    fcts_LR <- unlist(lapply(seq_along(xR), function(i) {
      pi <- p[[i]]
      xTi <- xT[i]
      xRi <- xR[i]
      
      calc_LR_den_Ha_single_no_checks_wTwR(
        xT = xTi, xR = xRi, 
        wT = wT, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
    }))
    
    prod(fcts_LR)
  })) |> mean()
  
  WoE <- log10(LR_Hp_mc) - log10(LR_Ha_mc)
  
  return(WoE)
}

#' Calculate WoE for sample-specific error probabilities integrated over the donor prior distribution using numerical integration
#' 
#' Note that the numerical integration can have problems for prior distributions with low variance. 
#' Remember to verify results, e.g. by comparing to Monte Carlo integration by [calc_WoE_wTwR_integrate_wT_mc()].
#' 
#' @inherit calc_WoE_wTwR_integrate_wT_mc examples
#' 
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param shape1T_Hp under $H_p$ `wT` has beta prior on (0, 0.5) with parameters `shape1T_Hp` and `shape2T_Hp`
#' @param shape2T_Hp see `shape1T_Hp`
#' @param shape1T_Ha under $H_a$ `wT` has beta prior on (0, 0.5) with parameters `shape1T_Ha` and `shape2T_Ha`
#' @param shape2T_Ha see `shape1T_Ha`
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' 
#' @importFrom pracma quad
#'
#' @export
calc_WoE_wTwR_integrate_wT_num <- function(xT, xR, shape1T_Hp, shape2T_Hp, shape1T_Ha, shape2T_Ha, wR, p) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  
  LR_Hp_i_num <- function(wT) {
    fcts_LR <- unlist(lapply(seq_along(xR), function(i) {
      pi <- p[[i]]
      xTi <- xT[i]
      xRi <- xR[i]
      
      calc_LR_num_Hp_single_no_checks_wTwR(
        xT = xTi, xR = xRi, 
        wT = wT, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
    }))
    
    z <- dbeta05(wT, shape1 = shape1T_Hp, shape2 = shape2T_Hp)
    z * prod(fcts_LR)
  }
  # LR_num <- integrate(LR_Hp_i_num, lower = 0, upper = 0.5, 
  #                     rel.tol = 1e-12, 
  #                     abs.tol = 1e-12, 
  #                     subdivisions = 1000L)
  
  # Guided integration
  xs <- qbeta05(seq(0, 1, by = 0.1), shape1 = shape1T_Hp, shape2 = shape2T_Hp)
  LR_num <- lapply(seq_len(length(xs) - 1L), \(i) {
    # integrate(LR_Hp_i_num, lower = xs[i], upper = xs[i+1L], 
    #                               rel.tol = 1e-12,
    #                               abs.tol = 1e-12,
    #                               subdivisions = 1000L)$value
    quad(LR_Hp_i_num, xa = xs[i], xb = xs[i+1L], tol = 1e-14, trace = FALSE)
    #quadgk(LR_Hp_i_num, a = xs[i], b = xs[i+1L], tol = 1e-14)
  }) |> unlist() |> sum()
  
  if (FALSE) {
    LR_Hp_i_num(shape1T_Hp/(2*(shape1T_Hp + shape2T_Hp)))
    LR_num
  }
  
  
  
  LR_Ha_i_num <- function(wT) {
    fcts_LR <- unlist(lapply(seq_along(xR), function(i) {
      pi <- p[[i]]
      xTi <- xT[i]
      xRi <- xR[i]
      
      calc_LR_den_Ha_single_no_checks_wTwR(
        xT = xTi, xR = xRi, 
        wT = wT, 
        wR = wR,
        p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
    }))
    
    z <- dbeta05(wT, shape1 = shape1T_Ha, shape2 = shape2T_Ha)
    z * prod(fcts_LR)
  }
  #LR_Ha_i_num(0.01)
  # LR_den <- integrate(LR_Ha_i_num, lower = 0, upper = 0.5, 
  #                     rel.tol = 1e-12, 
  #                     abs.tol = 1e-12, 
  #                     subdivisions = 1000L)
  # Guided integration
  xs <- qbeta05(seq(0, 1, by = 0.1), shape1 = shape1T_Ha, shape2 = shape2T_Ha)
  LR_den <- lapply(seq_len(length(xs) - 1L), \(i) {
    # integrate(LR_Ha_i_num, lower = xs[i], upper = xs[i+1L], 
    #           rel.tol = 1e-12,
    #           abs.tol = 1e-12,
    #           subdivisions = 1000L)$value
    quad(LR_Ha_i_num, xa = xs[i], xb = xs[i+1L], tol = 1e-14, trace = FALSE)
    #quadgk(LR_Ha_i_num, a = xs[i], b = xs[i+1L], tol = 1e-14)
  }) |> unlist() |> sum()
  
  if (FALSE) {
    LR_Ha_i_num(shape1T_Hp/(2*(shape1T_Hp + shape2T_Hp)))
    LR_den
  }
  
  if (FALSE) {
    log10(LR_Hp_i_num(shape1T_Hp/(2*(shape1T_Hp + shape2T_Hp)))) - 
      log10(LR_Ha_i_num(shape1T_Hp/(2*(shape1T_Hp + shape2T_Hp))))
  }
  
  #WoE <- log10(LR_num$value) - log10(LR_den$value)
  WoE <- log10(LR_num) - log10(LR_den)
  
  return(WoE)
}























#' Calculate WoE for sample-specific error probabilities using profile likelihood using numerical optimisation
#' 
#' @examples
#' calc_LRs_wTwR(xT = c(0, 0), xR = c(0, 1), wT = 1e-2, wR = 1e-5, p = c(0.25, 0.25, 0.5)) |> log10() |> sum()
#' 
#' calc_WoE_wTwR_profilemax_wT_num(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' 
#' shpT <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-7, a = 0, b = 0.5)
#' # curve(dbeta05(x, shpT[1], shpT[2]), from = 0, to = 0.1, n = 1001)
#' calc_WoE_wTwR_integrate_wT_mc(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1T_Hp = shpT[1], 
#'   shape2T_Hp = shpT[2],
#'   shape1T_Ha = shpT[1], 
#'   shape2T_Ha = shpT[2],
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 1000)
#'   
#' calc_WoE_wTwR_integrate_wT_num(
#'   xT = c(0, 0), 
#'   xR = c(0, 1), 
#'   shape1T_Hp = shpT[1], 
#'   shape2T_Hp = shpT[2],
#'   shape1T_Ha = shpT[1], 
#'   shape2T_Ha = shpT[2],
#'   wR = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#'   
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#'
#' @export
calc_WoE_wTwR_profilemax_wT_num <- function(xT, xR, wR, p) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))

  # Hp
  calc_num_Hp <- function(wT) {
    lik <- unlist(lapply(seq_along(xT), \(i) {
      pi <- p[[i]]
      liki <- wgsLR:::calc_LR_num_Hp_single_no_checks_wTwR(xT = xT[i], xR = xR[i], wT = wT, wR = wR, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      log10(liki)
    }))
    
    sum(lik)
  }
  #calc_num_Hp(1e-3)
  opt_res_Hp <- optimise(calc_num_Hp, maximum = TRUE, interval = c(wR, 0.5), tol = 1e-14)
  #opt_res_Hp

  
  # Hd
  calc_den_Ha <- function(wT) {
    lik <- unlist(lapply(seq_along(xT), \(i) {
      pi <- p[[i]]
      liki <- wgsLR:::calc_LR_den_Ha_single_no_checks_wTwR(xT = xT[i], xR = xR[i], wT = wT, wR = wR, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      log10(liki)
    }))
    
    sum(lik)
  }
  #calc_den_Ha(1e-3)
  opt_res_Ha <- optimise(calc_den_Ha, maximum = TRUE, interval = c(wR, 0.5), tol = 1e-14)
  #opt_res_Ha
  
  if (FALSE) {
    wT <- seq(wR, 0.5, length.out = 1001)
    y_Hp <- lapply(wT, calc_num_Hp) |> unlist()
    y_Ha <- lapply(wT, calc_den_Ha) |> unlist()
    
    plot(wT, y_Hp, ylim = range(c(y_Hp, y_Ha)), col = "blue", type = "l")
    lines(wT, y_Ha, col = "red", type = "l")
    
    calc_num_Hp(opt_res_Hp$maximum)
    calc_den_Ha(wR); calc_den_Ha(opt_res_Hp$maximum); calc_den_Ha(0.5)
  }
  
  WoE <- opt_res_Hp$objective - opt_res_Ha$objective
  
  return(list(wT_Hp = opt_res_Hp$maximum,
              wT_Ha = opt_res_Ha$maximum,
              log10PrE_Hp = opt_res_Hp$objective,
              log10PrE_Ha = opt_res_Ha$objective,
              WoE = WoE))
}

