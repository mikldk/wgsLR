if (FALSE) {
  # Generate code for calc_LR_single_no_checks_w()
  
  library(tidyverse)
  for (xD in 0L:2L) {
    for (xS in 0L:2L) {
      
      e <- d_prob_LR_w |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
      
      condition <- paste0("else if (xD == ", xD, "L && xS == ", xS, "L) ")
      
      if (xD == 0L && xS == 0L) {
        condition <- paste0("if (xD == ", xD, "L && xS == ", xS, "L) ")
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

calc_LR_single_no_checks_w <- function(xD, xS, w, p_0, p_1, p_2) {
  if (xD == 0L && xS == 0L)   {
    return( (p_0*(1 - w)^4 + p_1*w^2*(1 - w)^2 + p_2*w^4)/(p_0^2*(1 - w)^4 + 2*p_0*p_1*w*(1 - w)^3 + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w^3*(1 - w) + p_2^2*w^4) )
  }
  else if (xD == 0L && xS == 1L)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xD == 0L && xS == 2L)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 1L && xS == 0L)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xD == 1L && xS == 1L)   {
    return( (4*p_0*w^2*(1 - w)^2 + p_1*w^4 + 2*p_1*w^2*(1 - w)^2 + p_1*(1 - w)^4 + 4*p_2*w^2*(1 - w)^2)/(4*p_0^2*w^2*(1 - w)^2 + 4*p_0*p_1*w^3*(1 - w) + 4*p_0*p_1*w*(1 - w)^3 + 8*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^4 + 2*p_1^2*w^2*(1 - w)^2 + p_1^2*(1 - w)^4 + 4*p_1*p_2*w^3*(1 - w) + 4*p_1*p_2*w*(1 - w)^3 + 4*p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 1L && xS == 2L)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xD == 2L && xS == 0L)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 2L && xS == 1L)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xD == 2L && xS == 2L)   {
    return( (p_0*w^4 + p_1*w^2*(1 - w)^2 + p_2*(1 - w)^4)/(p_0^2*w^4 + 2*p_0*p_1*w^3*(1 - w) + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w*(1 - w)^3 + p_2^2*(1 - w)^4) )
  }
  else {
    stop("Not recognised")
  }
}


calc_LRs_no_checks_w <- function(xD, xS, w, p) {
  unlist(lapply(seq_along(xS), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_w(xS = xS[i], xD = xD[i], 
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
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param w error probability
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_w <- function(xD, xS, w, p) {
  xS <- check_x(xS)
  xD <- check_x(xD)
  stopifnot(length(xS) == length(xD))
  
  check_w(w)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  LRs <- calc_LRs_no_checks_w(xD = xD, xS = xS, w = w, p = p)
  return(LRs)
}


############################################


if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wDwS()
  
  library(tidyverse)
  for (xD in 0L:2L) {
    for (xS in 0L:2L) {
      
      e <- d_prob_LR_wDwS |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
      
      condition <- paste0("else if (xD == ", xD, "L && xS == ", xS, "L) ")
      
      if (xD == 0L && xS == 0L) {
        condition <- paste0("if (xD == ", xD, "L && xS == ", xS, "L) ")
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

calc_LR_single_no_checks_wDwS <- function(xD, xS, wD, wS, p_0, p_1, p_2) {
  if (xD == 0L && xS == 0L)   {
    return( (p_0*(wD - 1)^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*wS^2)/(p_0^2*(wD - 1)^2*(wS - 1)^2 - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) + p_2^2*wD^2*wS^2) )
  }
  else if (xD == 0L && xS == 1L)   {
    return( (2*p_0*wS*(wD - 1)^2*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wD^2*wS*(wS - 1))/(2*p_0^2*wS*(wD - 1)^2*(wS - 1) - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wD^2*wS^2 - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + 2*p_2^2*wD^2*wS*(wS - 1)) )
  }
  else if (xD == 0L && xS == 2L)   {
    return( (p_0*wS^2*(wD - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*(wS - 1)^2)/(p_0^2*wS^2*(wD - 1)^2 - p_0*p_1*wD*wS^2*(wD - 1) - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + p_2^2*wD^2*(wS - 1)^2) )
  }
  else if (xD == 1L && xS == 0L)   {
    return( (2*p_0*wD*(wD - 1)*(wS - 1)^2 + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*wS^2*(wD - 1))/(2*p_0^2*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*wS^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 + 2*p_2^2*wD*wS^2*(wD - 1)) )
  }
  else if (xD == 1L && xS == 1L)   {
    return( (-4*p_0*wD*wS*(wD - 1)*(wS - 1) - p_1*wD^2*wS^2 - p_1*wD^2*(wS - 1)^2 - p_1*wS^2*(wD - 1)^2 - p_1*(wD - 1)^2*(wS - 1)^2 - 4*p_2*wD*wS*(wD - 1)*(wS - 1))/(-4*p_0^2*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_1*wD^2*wS*(wS - 1) + 2*p_0*p_1*wD*wS^2*(wD - 1) + 2*p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_0*p_1*wS*(wD - 1)^2*(wS - 1) - 8*p_0*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1^2*wD^2*wS^2 - p_1^2*wD^2*(wS - 1)^2 - p_1^2*wS^2*(wD - 1)^2 - p_1^2*(wD - 1)^2*(wS - 1)^2 + 2*p_1*p_2*wD^2*wS*(wS - 1) + 2*p_1*p_2*wD*wS^2*(wD - 1) + 2*p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + 2*p_1*p_2*wS*(wD - 1)^2*(wS - 1) - 4*p_2^2*wD*wS*(wD - 1)*(wS - 1)) )
  }
  else if (xD == 1L && xS == 2L)   {
    return( (2*p_0*wD*wS^2*(wD - 1) + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*(wD - 1)*(wS - 1)^2)/(2*p_0^2*wD*wS^2*(wD - 1) - p_0*p_1*wD^2*wS^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wD*(wD - 1)*(wS - 1)^2) )
  }
  else if (xD == 2L && xS == 0L)   {
    return( (p_0*wD^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wS^2*(wD - 1)^2)/(p_0^2*wD^2*(wS - 1)^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*wS^2*(wD - 1)^2) )
  }
  else if (xD == 2L && xS == 1L)   {
    return( (2*p_0*wD^2*wS*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wS*(wD - 1)^2*(wS - 1))/(2*p_0^2*wD^2*wS*(wS - 1) - p_0*p_1*wD^2*wS^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wS*(wD - 1)^2*(wS - 1)) )
  }
  else if (xD == 2L && xS == 2L)   {
    return( (p_0*wD^2*wS^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*(wD - 1)^2*(wS - 1)^2)/(p_0^2*wD^2*wS^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*wS^2*(wD - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*(wD - 1)^2*(wS - 1)^2) )
  }
  else {
    stop("xD/xS: Not recognised")
  }
}

calc_LRs_no_checks_wDwS <- function(xD, xS, wD, wS, p) {
  unlist(lapply(seq_along(xS), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_wDwS(xS = xS[i], xD = xD[i], 
                                  wD = wD, 
                                  wS = wS,
                                  p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
  }))
}

#' Calculate LR for a profile for sample-specific error probabilities
#' 
#' @examples
#' calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wDwS(c(0, 0), c(0, 0), wD = 0, wS = 0, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#' calc_LRs_w(c(0, 0), c(0, 0), w = 0, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-5, wS = 1e-5, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-5, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-3, wS = 1e-3, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 1e-3, p = c(0.25, 0.25, 0.5))
#' 
#' calc_LRs_wDwS(c(0, 0), c(0, 1), wD = 1e-2, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#' calc_LRs_w(c(0, 0), c(0, 1), w = 5e-3, p = c(0.25, 0.25, 0.5))
#' 
#' @param xD profile from case (of 0, 1, 2)
#' @param xS profile from suspect (of 0, 1, 2)
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_wDwS <- function(xD, xS, wD, wS, p) {
  xD <- check_x(xD)
  xS <- check_x(xS)
  
  stopifnot(length(xS) == length(xD))
  
  check_w(wD)
  check_w(wS)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xS))
  check_p(p)
  stopifnot(length(p) == length(xD))
  
  LRs <- calc_LRs_no_checks_wDwS(xD = xD, xS = xS, wD = wD, wS = wS, p = p)
  return(LRs)
}

