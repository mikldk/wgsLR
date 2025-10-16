if (FALSE) {
  # Generate code for calc_LR_single_no_checks_w()
  
  library(tidyverse)
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      
      e <- d_LR_w |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      
      condition <- paste0("else if (xT == ", xT, "L && xR == ", xR, "L) ")
      
      if (xT == 0L && xR == 0L) {
        condition <- paste0("if (xT == ", xT, "L && xR == ", xR, "L) ")
      } 
      cat(condition, " {
            return(", e, ")
          }\n")
    }
  }
  
  cat(" else {
    stop(\"Not recognised\")
  }")
}

calc_LR_single_no_checks_w <- function(xT, xR, w, p_0, p_1, p_2) {
  if (xT == 0L && xR == 0L)   {
    return( (p_0*(1 - w)^4 + p_1*w^2*(1 - w)^2 + p_2*w^4)/(p_0^2*(1 - w)^4 + 2*p_0*p_1*w*(1 - w)^3 + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w^3*(1 - w) + p_2^2*w^4) )
  }
  else if (xT == 0L && xR == 1L)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xT == 0L && xR == 2L)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xT == 1L && xR == 0L)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xT == 1L && xR == 1L)   {
    return( (4*p_0*w^2*(1 - w)^2 + p_1*w^4 + 2*p_1*w^2*(1 - w)^2 + p_1*(1 - w)^4 + 4*p_2*w^2*(1 - w)^2)/(4*p_0^2*w^2*(1 - w)^2 + 4*p_0*p_1*w^3*(1 - w) + 4*p_0*p_1*w*(1 - w)^3 + 8*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^4 + 2*p_1^2*w^2*(1 - w)^2 + p_1^2*(1 - w)^4 + 4*p_1*p_2*w^3*(1 - w) + 4*p_1*p_2*w*(1 - w)^3 + 4*p_2^2*w^2*(1 - w)^2) )
  }
  else if (xT == 1L && xR == 2L)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xT == 2L && xR == 0L)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xT == 2L && xR == 1L)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xT == 2L && xR == 2L)   {
    return( (p_0*w^4 + p_1*w^2*(1 - w)^2 + p_2*(1 - w)^4)/(p_0^2*w^4 + 2*p_0*p_1*w^3*(1 - w) + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w*(1 - w)^3 + p_2^2*(1 - w)^4) )
  }
  else {
    stop("Not recognised")
  }
}


calc_LRs_no_checks_w <- function(xT, xR, w, p) {
  unlist(lapply(seq_along(xR), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_w(xR = xR[i], xT = xT[i], 
                               w = w, 
                               p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
  }))
}

#' Calculate LR for a profile for one error probability, $w$
#' 
#' @examples
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' calc_LRs_w(c(0, 0), c(0, 1), w = 0, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#' 
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param w error probability
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_w <- function(xT, xR, w, p) {
  xR <- check_x(xR)
  xT <- check_x(xT)
  stopifnot(length(xR) == length(xT))
  
  check_w(w)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  LRs <- calc_LRs_no_checks_w(xT = xT, xR = xR, w = w, p = p)
  return(LRs)
}


############################################


if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wTwR()
  
  library(tidyverse)
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      
      e <- d_LR_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      
      condition <- paste0("else if (xT == ", xT, "L && xR == ", xR, "L) ")
      
      if (xT == 0L && xR == 0L) {
        condition <- paste0("if (xT == ", xT, "L && xR == ", xR, "L) ")
      } 
      cat(condition, " {
            return(", e, ")
          }\n")
    }
  }
  
  cat(" else {
    stop(\"Not recognised\")
  }")
}

calc_LR_single_no_checks_wTwR <- function(xT, xR, wT, wR, p_0, p_1, p_2) {
  if (xT == 0L && xR == 0L)   {
    return( (p_0*(wT - 1)^2*(wR - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wT^2*wR^2)/(p_0^2*(wT - 1)^2*(wR - 1)^2 - p_0*p_1*wT*(wT - 1)*(wR - 1)^2 - p_0*p_1*wR*(wT - 1)^2*(wR - 1) + p_0*p_2*wT^2*(wR - 1)^2 + p_0*p_2*wR^2*(wT - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT^2*wR*(wR - 1) - p_1*p_2*wT*wR^2*(wT - 1) + p_2^2*wT^2*wR^2) )
  }
  else if (xT == 0L && xR == 1L)   {
    return( (2*p_0*wR*(wT - 1)^2*(wR - 1) + p_1*wT*wR^2*(wT - 1) + p_1*wT*(wT - 1)*(wR - 1)^2 + 2*p_2*wT^2*wR*(wR - 1))/(2*p_0^2*wR*(wT - 1)^2*(wR - 1) - 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) - p_0*p_1*wR^2*(wT - 1)^2 - p_0*p_1*(wT - 1)^2*(wR - 1)^2 + 2*p_0*p_2*wT^2*wR*(wR - 1) + 2*p_0*p_2*wR*(wT - 1)^2*(wR - 1) + p_1^2*wT*wR^2*(wT - 1) + p_1^2*wT*(wT - 1)*(wR - 1)^2 - p_1*p_2*wT^2*wR^2 - p_1*p_2*wT^2*(wR - 1)^2 - 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) + 2*p_2^2*wT^2*wR*(wR - 1)) )
  }
  else if (xT == 0L && xR == 2L)   {
    return( (p_0*wR^2*(wT - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wT^2*(wR - 1)^2)/(p_0^2*wR^2*(wT - 1)^2 - p_0*p_1*wT*wR^2*(wT - 1) - p_0*p_1*wR*(wT - 1)^2*(wR - 1) + p_0*p_2*wT^2*wR^2 + p_0*p_2*(wT - 1)^2*(wR - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT^2*wR*(wR - 1) - p_1*p_2*wT*(wT - 1)*(wR - 1)^2 + p_2^2*wT^2*(wR - 1)^2) )
  }
  else if (xT == 1L && xR == 0L)   {
    return( (2*p_0*wT*(wT - 1)*(wR - 1)^2 + p_1*wT^2*wR*(wR - 1) + p_1*wR*(wT - 1)^2*(wR - 1) + 2*p_2*wT*wR^2*(wT - 1))/(2*p_0^2*wT*(wT - 1)*(wR - 1)^2 - p_0*p_1*wT^2*(wR - 1)^2 - 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) - p_0*p_1*(wT - 1)^2*(wR - 1)^2 + 2*p_0*p_2*wT*wR^2*(wT - 1) + 2*p_0*p_2*wT*(wT - 1)*(wR - 1)^2 + p_1^2*wT^2*wR*(wR - 1) + p_1^2*wR*(wT - 1)^2*(wR - 1) - p_1*p_2*wT^2*wR^2 - 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wR^2*(wT - 1)^2 + 2*p_2^2*wT*wR^2*(wT - 1)) )
  }
  else if (xT == 1L && xR == 1L)   {
    return( (-4*p_0*wT*wR*(wT - 1)*(wR - 1) - p_1*wT^2*wR^2 - p_1*wT^2*(wR - 1)^2 - p_1*wR^2*(wT - 1)^2 - p_1*(wT - 1)^2*(wR - 1)^2 - 4*p_2*wT*wR*(wT - 1)*(wR - 1))/(-4*p_0^2*wT*wR*(wT - 1)*(wR - 1) + 2*p_0*p_1*wT^2*wR*(wR - 1) + 2*p_0*p_1*wT*wR^2*(wT - 1) + 2*p_0*p_1*wT*(wT - 1)*(wR - 1)^2 + 2*p_0*p_1*wR*(wT - 1)^2*(wR - 1) - 8*p_0*p_2*wT*wR*(wT - 1)*(wR - 1) - p_1^2*wT^2*wR^2 - p_1^2*wT^2*(wR - 1)^2 - p_1^2*wR^2*(wT - 1)^2 - p_1^2*(wT - 1)^2*(wR - 1)^2 + 2*p_1*p_2*wT^2*wR*(wR - 1) + 2*p_1*p_2*wT*wR^2*(wT - 1) + 2*p_1*p_2*wT*(wT - 1)*(wR - 1)^2 + 2*p_1*p_2*wR*(wT - 1)^2*(wR - 1) - 4*p_2^2*wT*wR*(wT - 1)*(wR - 1)) )
  }
  else if (xT == 1L && xR == 2L)   {
    return( (2*p_0*wT*wR^2*(wT - 1) + p_1*wT^2*wR*(wR - 1) + p_1*wR*(wT - 1)^2*(wR - 1) + 2*p_2*wT*(wT - 1)*(wR - 1)^2)/(2*p_0^2*wT*wR^2*(wT - 1) - p_0*p_1*wT^2*wR^2 - 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) - p_0*p_1*wR^2*(wT - 1)^2 + 2*p_0*p_2*wT*wR^2*(wT - 1) + 2*p_0*p_2*wT*(wT - 1)*(wR - 1)^2 + p_1^2*wT^2*wR*(wR - 1) + p_1^2*wR*(wT - 1)^2*(wR - 1) - p_1*p_2*wT^2*(wR - 1)^2 - 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*(wT - 1)^2*(wR - 1)^2 + 2*p_2^2*wT*(wT - 1)*(wR - 1)^2) )
  }
  else if (xT == 2L && xR == 0L)   {
    return( (p_0*wT^2*(wR - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wR^2*(wT - 1)^2)/(p_0^2*wT^2*(wR - 1)^2 - p_0*p_1*wT^2*wR*(wR - 1) - p_0*p_1*wT*(wT - 1)*(wR - 1)^2 + p_0*p_2*wT^2*wR^2 + p_0*p_2*(wT - 1)^2*(wR - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT*wR^2*(wT - 1) - p_1*p_2*wR*(wT - 1)^2*(wR - 1) + p_2^2*wR^2*(wT - 1)^2) )
  }
  else if (xT == 2L && xR == 1L)   {
    return( (2*p_0*wT^2*wR*(wR - 1) + p_1*wT*wR^2*(wT - 1) + p_1*wT*(wT - 1)*(wR - 1)^2 + 2*p_2*wR*(wT - 1)^2*(wR - 1))/(2*p_0^2*wT^2*wR*(wR - 1) - p_0*p_1*wT^2*wR^2 - p_0*p_1*wT^2*(wR - 1)^2 - 2*p_0*p_1*wT*wR*(wT - 1)*(wR - 1) + 2*p_0*p_2*wT^2*wR*(wR - 1) + 2*p_0*p_2*wR*(wT - 1)^2*(wR - 1) + p_1^2*wT*wR^2*(wT - 1) + p_1^2*wT*(wT - 1)*(wR - 1)^2 - 2*p_1*p_2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wR^2*(wT - 1)^2 - p_1*p_2*(wT - 1)^2*(wR - 1)^2 + 2*p_2^2*wR*(wT - 1)^2*(wR - 1)) )
  }
  else if (xT == 2L && xR == 2L)   {
    return( (p_0*wT^2*wR^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*(wT - 1)^2*(wR - 1)^2)/(p_0^2*wT^2*wR^2 - p_0*p_1*wT^2*wR*(wR - 1) - p_0*p_1*wT*wR^2*(wT - 1) + p_0*p_2*wT^2*(wR - 1)^2 + p_0*p_2*wR^2*(wT - 1)^2 + p_1^2*wT*wR*(wT - 1)*(wR - 1) - p_1*p_2*wT*(wT - 1)*(wR - 1)^2 - p_1*p_2*wR*(wT - 1)^2*(wR - 1) + p_2^2*(wT - 1)^2*(wR - 1)^2) )
  }
  else {
    stop("xT/xR: Not recognised")
  }
}

calc_LRs_no_checks_wTwR <- function(xT, xR, wT, wR, p) {
  unlist(lapply(seq_along(xR), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_wTwR(xR = xR[i], xT = xT[i], 
                                  wT = wT, 
                                  wR = wR,
                                  p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
  }))
}

#' Calculate LR for a profile for sample-specific error probabilities
#' 
#' @examples
#' calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wTwR(c(0, 0), c(0, 0), wT = 0, wR = 0, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 0, wR = 0, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-5, wR = 1e-5, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-3, wR = 1e-3, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wTwR(c(0, 0), c(0, 1), wT = 1e-2, wR = 1e-6, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 5e-3, p = c(0.25, 0.25, 0.5))
#' 
#' @param xT profile from case (of 0, 1, 2)
#' @param xR profile from suspect (of 0, 1, 2)
#' @param wT error probability for donor sample
#' @param wR error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xT`/`xR`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_wTwR <- function(xT, xR, wT, wR, p) {
  xT <- check_x(xT)
  xR <- check_x(xR)
  
  stopifnot(length(xR) == length(xT))
  
  check_w(wT)
  check_w(wR)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xR))
  check_p(p)
  stopifnot(length(p) == length(xT))
  
  LRs <- calc_LRs_no_checks_wTwR(xT = xT, xR = xR, wT = wT, wR = wR, p = p)
  return(LRs)
}

