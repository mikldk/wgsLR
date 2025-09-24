if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wDwS()
  
  library(tidyverse)
  library(caracas)
  
  
  code <- ""
  for (xD in 0L:2L) {
    for (xS in 0L:2L) {
      
      e <- d_prob_Hp_wDwS |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
      #e <- d_prob_Hd_wDwS |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
      
      condition <- paste0("else if (xD == ", xD, "L && xS == ", xS, "L) ")
      
      if (xD == 0L && xS == 0L) {
        condition <- paste0("if (xD == ", xD, "L && xS == ", xS, "L) ")
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

calc_LR_num_Hp_single_no_checks_wDwS <- function(xD, xS, wD, wS, p_0, p_1, p_2) {
  if (xD == 0L && xS == 0L)  {
    return(p_0*(wD - 1)^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*wS^2)
  }
  else if (xD == 0L && xS == 1L)  {
    return(-2*p_0*wS*(wD - 1)^2*(wS - 1) - p_1*wD*wS^2*(wD - 1) - p_1*wD*(wD - 1)*(wS - 1)^2 - 2*p_2*wD^2*wS*(wS - 1))
  }
  else if (xD == 0L && xS == 2L)  {
    return(p_0*wS^2*(wD - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*(wS - 1)^2)
  }
  else if (xD == 1L && xS == 0L)  {
    return(-2*p_0*wD*(wD - 1)*(wS - 1)^2 - p_1*wD^2*wS*(wS - 1) - p_1*wS*(wD - 1)^2*(wS - 1) - 2*p_2*wD*wS^2*(wD - 1))
  }
  else if (xD == 1L && xS == 1L)  {
    return(4*p_0*wD*wS*(wD - 1)*(wS - 1) + p_1*wD^2*wS^2 + p_1*wD^2*(wS - 1)^2 + p_1*wS^2*(wD - 1)^2 + p_1*(wD - 1)^2*(wS - 1)^2 + 4*p_2*wD*wS*(wD - 1)*(wS - 1))
  }
  else if (xD == 1L && xS == 2L)  {
    return(-2*p_0*wD*wS^2*(wD - 1) - p_1*wD^2*wS*(wS - 1) - p_1*wS*(wD - 1)^2*(wS - 1) - 2*p_2*wD*(wD - 1)*(wS - 1)^2)
  }
  else if (xD == 2L && xS == 0L)  {
    return(p_0*wD^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wS^2*(wD - 1)^2)
  }
  else if (xD == 2L && xS == 1L)  {
    return(-2*p_0*wD^2*wS*(wS - 1) - p_1*wD*wS^2*(wD - 1) - p_1*wD*(wD - 1)*(wS - 1)^2 - 2*p_2*wS*(wD - 1)^2*(wS - 1))
  }
  else if (xD == 2L && xS == 2L)  {
    return(p_0*wD^2*wS^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*(wD - 1)^2*(wS - 1)^2)
  }
  else {
    stop("Not recognised")
  }
}


calc_LR_den_Hd_single_no_checks_wDwS <- function(xD, xS, wD, wS, p_0, p_1, p_2) {
  if (xD == 0L && xS == 0L)  {
    return(p_0^2*(wD - 1)^2*(wS - 1)^2 - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) + p_2^2*wD^2*wS^2)
  }
  else if (xD == 0L && xS == 1L)  {
    return(-2*p_0^2*wS*(wD - 1)^2*(wS - 1) + 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + p_0*p_1*wS^2*(wD - 1)^2 + p_0*p_1*(wD - 1)^2*(wS - 1)^2 - 2*p_0*p_2*wD^2*wS*(wS - 1) - 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) - p_1^2*wD*wS^2*(wD - 1) - p_1^2*wD*(wD - 1)*(wS - 1)^2 + p_1*p_2*wD^2*wS^2 + p_1*p_2*wD^2*(wS - 1)^2 + 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - 2*p_2^2*wD^2*wS*(wS - 1))
  }
  else if (xD == 0L && xS == 2L)  {
    return(p_0^2*wS^2*(wD - 1)^2 - p_0*p_1*wD*wS^2*(wD - 1) - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + p_2^2*wD^2*(wS - 1)^2)
  }
  else if (xD == 1L && xS == 0L)  {
    return(-2*p_0^2*wD*(wD - 1)*(wS - 1)^2 + p_0*p_1*wD^2*(wS - 1)^2 + 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + p_0*p_1*(wD - 1)^2*(wS - 1)^2 - 2*p_0*p_2*wD*wS^2*(wD - 1) - 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 - p_1^2*wD^2*wS*(wS - 1) - p_1^2*wS*(wD - 1)^2*(wS - 1) + p_1*p_2*wD^2*wS^2 + 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + p_1*p_2*wS^2*(wD - 1)^2 - 2*p_2^2*wD*wS^2*(wD - 1))
  }
  else if (xD == 1L && xS == 1L)  {
    return(4*p_0^2*wD*wS*(wD - 1)*(wS - 1) - 2*p_0*p_1*wD^2*wS*(wS - 1) - 2*p_0*p_1*wD*wS^2*(wD - 1) - 2*p_0*p_1*wD*(wD - 1)*(wS - 1)^2 - 2*p_0*p_1*wS*(wD - 1)^2*(wS - 1) + 8*p_0*p_2*wD*wS*(wD - 1)*(wS - 1) + p_1^2*wD^2*wS^2 + p_1^2*wD^2*(wS - 1)^2 + p_1^2*wS^2*(wD - 1)^2 + p_1^2*(wD - 1)^2*(wS - 1)^2 - 2*p_1*p_2*wD^2*wS*(wS - 1) - 2*p_1*p_2*wD*wS^2*(wD - 1) - 2*p_1*p_2*wD*(wD - 1)*(wS - 1)^2 - 2*p_1*p_2*wS*(wD - 1)^2*(wS - 1) + 4*p_2^2*wD*wS*(wD - 1)*(wS - 1))
  }
  else if (xD == 1L && xS == 2L)  {
    return(-2*p_0^2*wD*wS^2*(wD - 1) + p_0*p_1*wD^2*wS^2 + 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + p_0*p_1*wS^2*(wD - 1)^2 - 2*p_0*p_2*wD*wS^2*(wD - 1) - 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 - p_1^2*wD^2*wS*(wS - 1) - p_1^2*wS*(wD - 1)^2*(wS - 1) + p_1*p_2*wD^2*(wS - 1)^2 + 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + p_1*p_2*(wD - 1)^2*(wS - 1)^2 - 2*p_2^2*wD*(wD - 1)*(wS - 1)^2)
  }
  else if (xD == 2L && xS == 0L)  {
    return(p_0^2*wD^2*(wS - 1)^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*wS^2*(wD - 1)^2)
  }
  else if (xD == 2L && xS == 1L)  {
    return(-2*p_0^2*wD^2*wS*(wS - 1) + p_0*p_1*wD^2*wS^2 + p_0*p_1*wD^2*(wS - 1)^2 + 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - 2*p_0*p_2*wD^2*wS*(wS - 1) - 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) - p_1^2*wD*wS^2*(wD - 1) - p_1^2*wD*(wD - 1)*(wS - 1)^2 + 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + p_1*p_2*wS^2*(wD - 1)^2 + p_1*p_2*(wD - 1)^2*(wS - 1)^2 - 2*p_2^2*wS*(wD - 1)^2*(wS - 1))
  }
  else if (xD == 2L && xS == 2L)  {
    return(p_0^2*wD^2*wS^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*wS^2*(wD - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*(wD - 1)^2*(wS - 1)^2)
  }
  else {
    stop("Not recognised")
  }
}







################################################################################


int_LR_num_Hp_single_no_checks_wDwS <- function(xD, xS, wD, wS, p_0, p_1, p_2, shape1D, shape2D) {
  if (xD == 0L && xS == 0L)  {
    return(int_Hp_xD0_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 0L && xS == 1L)  {
    return(int_Hp_xD0_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 0L && xS == 2L)  {
    return(int_Hp_xD0_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 0L)  {
    return(int_Hp_xD1_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 1L)  {
    return(int_Hp_xD1_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 2L)  {
    return(int_Hp_xD1_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 0L)  {
    return(int_Hp_xD2_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 1L)  {
    return(int_Hp_xD2_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 2L)  {
    return(int_Hp_xD2_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else {
    stop("Not recognised")
  }
}


int_LR_den_Hd_single_no_checks_wDwS <- function(xD, xS, wD, wS, p_0, p_1, p_2, shape1D, shape2D) {
  if (xD == 0L && xS == 0L)  {
    return(int_Hd_xD0_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 0L && xS == 1L)  {
    return(int_Hd_xD0_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 0L && xS == 2L)  {
    return(int_Hd_xD0_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 0L)  {
    return(int_Hd_xD1_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 1L)  {
    return(int_Hd_xD1_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 1L && xS == 2L)  {
    return(int_Hd_xD1_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 0L)  {
    return(int_Hd_xD2_xS0(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 1L)  {
    return(int_Hd_xD2_xS1(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else if (xD == 2L && xS == 2L)  {
    return(int_Hd_xD2_xS2(wS = wS, p_0 = p_0, p_1 = p_1, p_2 = p_2, a = shape1D, b = shape2D))
  }
  else {
    stop("Not recognised")
  }
}








#' Calculate LR for a profile for sample-specific error probabilities integrated over the prior distributions using Monte Carlo integration
#' 
#' @examples
#' calc_LRs_wDwS(xD = c(0, 0), xS = c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' # curve(dbeta05(x, shpD[1L], shpD[2L]), from = 0, to = 0.5)
#' shpS <- get_beta_parameters(mu = 1e-5, sigmasq = 2e-6, a = 0, b = 0.5)
#' # curve(dbeta05(x, shpS[1L], shpS[2L]), from = 0, to = 0.5)
#' 
#' calc_LRs_wDwS_integrate_wDwS_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   shape1S = shpS[1], shape2S = shpS[2], 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#'   
#' calc_LRs_wDwS_integrate_wDwS_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   shape1S = 1, shape2S = 1, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#' 
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param shape1D `wD` has beta prior (with support on 0-0.5) with parameters `shape1D` and `shape2D`
#' @param shape2D see `shape1D`
#' @param shape1S `wS` has beta prior (with support on 0-0.5) with parameters `shape1S` and `shape2S`
#' @param shape2S see `shape1S`
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' @param n_samples number of random samples from each prior distribution
#'
#' @export
calc_LRs_wDwS_integrate_wDwS_mc <- function(xD, xS, shape1D, shape2D, shape1S, shape2S, p, n_samples = 1000) {
  xD <- check_x(xD)
  xS <- check_x(xS)
  
  stopifnot(length(xS) == length(xD))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  wDs <- rbeta05(n_samples, shape1 = shape1D, shape2 = shape2D)
  wSs <- rbeta05(n_samples, shape1 = shape1S, shape2 = shape2S)
  
  n2 <- n_samples*n_samples
  
  LR <- rep(0, length(p))
  for (wD in wDs) {
    for (wS in wSs) {
      LR <- LR + calc_LRs_no_checks_wDwS(xD = xD, xS = xS, wD = wD, wS = wS, p = p)/n2
    }
  }
  
  return(LR)
}



#' Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using Monte Carlo integration
#' 
#' @examples
#' calc_LRs_wDwS(xD = c(0, 0), xS = c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' shpS <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-7, a = 0, b = 0.5)
#' integrate(function(x) dbeta05(x, shape1 = shpS[1L], shape2 = shpS[2L]), lower = 1e-12, upper = 0.5, rel.tol = 1e-6)
#' calc_LRs_wDwS_integrate_wDwS_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   shape1S = shpS[1], shape2S = shpS[2], 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, shpS[1], shpS[2]), from = 0, to = 0.5)
#' 
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param shape1D `wD` has beta prior (with support on 0-0.5) with parameters `shape1D` and `shape2D`
#' @param shape2D see `shape1D`
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' @param n_samples number of random samples from each prior distribution
#'
#' @export
calc_LRs_wDwS_integrate_wD_mc <- function(xD, xS, shape1D, shape2D, wS, p, n_samples = 1000) {
  xD <- check_x(xD)
  xS <- check_x(xS)
  
  stopifnot(length(xS) == length(xD))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  wDs <- rbeta05(n_samples, shape1 = shape1D, shape2 = shape2D)
  
  LR <- rep(0, length(p))
  for (wD in wDs) {
    LR <- LR + calc_LRs_no_checks_wDwS(xD = xD, xS = xS, wD = wD, wS = wS, p = p)/n_samples
  }
  
  return(LR)
}

#' Calculate LR for a profile for sample-specific error probabilities integrated over the donor prior distribution using numerical integration
#' 
#' @examples
#' calc_LRs_wDwS(xD = c(0, 0), xS = c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wDwS_integrate_wD_num(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' 
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wDwS_integrate_wD_num(
#'   xD = c(0, 0),
#'   xS = c(0, 1),
#'   shape1D = 1, shape2D = 1,
#'   wS = 1e-5,
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param shape1D_H1 Under $H_1$ (in $LR$'s numerator), `wD` has beta prior on (0, 0.5) with parameters `shape1D_H1` and `shape2D_H1`
#' @param shape2D_H1 see `shape1D_Hp`
#' @param shape1D_H2 Under $H_2$ (in $LR$'s denominator), `wD` has beta prior (on 0-0.5) with parameters `shape1D_H2` and `shape2D_H2`
#' @param shape2D_H2 see `shape1D_H2`
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' @param lower_int lowest value to integrate from; should be 0, but numerical instability can make this problematic
#' @param \dots pass on to [integrate()], e.g., `rel.tol` and `abs.tol`
#'
#' @export
calc_LRs_wDwS_integrate_wD_num <- function(xD, xS, shape1D_H1, shape2D_H1, shape1D_H2, shape2D_H2, wS, p, lower_int = 0, ...) {
  xD <- check_x(xD)
  xS <- check_x(xS)
  
  stopifnot(length(xS) == length(xD))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  LRs <- lapply(seq_along(p), \(i) {
    xDi <- xD[i]
    xSi <- xS[i]
    pi <- p[[i]]
    
    f_num <- function(wD) {
      LR_num <- calc_LR_num_Hp_single_no_checks_wDwS(xD = xDi, 
                                                     xS = xSi,
                                                     wD = wD, 
                                                     wS = wS,
                                                     p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      z <- dbeta05(wD, shape1 = shape1D_H1, shape2 = shape2D_H1, log = TRUE) + log(LR_num)
      exp(z)
    }
    
    f_den <- function(wD) {
      LR_den <- calc_LR_den_Hd_single_no_checks_wDwS(xD = xDi, 
                                                     xS = xSi,
                                                     wD = wD, 
                                                     wS = wS,
                                                     p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
      
      z <- dbeta05(wD, shape1 = shape1D_H2, shape2 = shape2D_H2, log = TRUE) + log(LR_den)
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
#' @examples
#' calc_LRs_wDwS(xD = c(0, 0), xS = c(0, 1), wD = 1e-2, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' shpD <- get_beta_parameters(mu = 1e-2, sigmasq = 2*1e-3, a = 0, b = 0.5)
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wDwS_integrate_wD_num(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = shpD[1], shape2D = shpD[2], 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5))
#' # curve(dbeta05(x, shpD[1], shpD[2]), from = 0, to = 0.5)
#' 
#' calc_LRs_wDwS_integrate_wD_mc(
#'   xD = c(0, 0), 
#'   xS = c(0, 1), 
#'   shape1D = 1, shape2D = 1, 
#'   wS = 1e-5, 
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' calc_LRs_wDwS_integrate_wD_num(
#'   xD = c(0, 0),
#'   xS = c(0, 1),
#'   shape1D = 1, shape2D = 1,
#'   wS = 1e-5,
#'   p = c(0.25, 0.25, 0.5),
#'   n_samples = 100)
#' # curve(dbeta05(x, 1, 1), from = 0, to = 0.5)
#'   
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param shape1D_H1 Under $H_1$ (in $LR$'s numerator), `wD` has beta prior on (0, 0.5) with parameters `shape1D_H1` and `shape2D_H1`
#' @param shape2D_H1 see `shape1D_Hp`
#' @param shape1D_H2 Under $H_2$ (in $LR$'s denominator), `wD` has beta prior (on 0-0.5) with parameters `shape1D_H2` and `shape2D_H2`
#' @param shape2D_H2 see `shape1D_H2`
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' @param use_mpfr use higher precision numbers via the `Rmpfr` package
#' @param stop_on_infinite stop if infinite numbers are encountered (if so, try `use_mpfr = TRUE`)
#' 
#' @importFrom Rmpfr beta
#' 
#' @export
calc_LRs_wDwS_integrate_wD <- function(xD, xS, shape1D_H1, shape2D_H1, shape1D_H2, shape2D_H2, wS, p, use_mpfr = TRUE, stop_on_infinite = TRUE) {
  xD <- check_x(xD)
  xS <- check_x(xS)
  
  stopifnot(length(xS) == length(xD))
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  if (use_mpfr) {
    shape1D_H1 <- mpfr(shape1D_H1, precBits = 256)
    shape2D_H1 <- mpfr(shape2D_H1, precBits = 256)
    
    shape1D_H2 <- mpfr(shape1D_H2, precBits = 256)
    shape2D_H2 <- mpfr(shape2D_H2, precBits = 256)
    
    wS <- mpfr(wS, precBits = 256)
  }
  
  LRs <- lapply(seq_along(p), \(i) {
    xDi <- xD[i]
    xSi <- xS[i]
    pi <- p[[i]]
    
    if (use_mpfr) {
      pi <- mpfr(pi, precBits = 256)
    }
    
    LR_num <- int_LR_num_Hp_single_no_checks_wDwS(xD = xDi, xS = xSi, wS = wS, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L], shape1D = shape1D_H1, shape2D = shape2D_H1)
    LR_den <- int_LR_den_Hd_single_no_checks_wDwS(xD = xDi, xS = xSi, wS = wS, p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L], shape1D = shape1D_H2, shape2D = shape2D_H2)
    
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

