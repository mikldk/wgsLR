if (FALSE) {
  # Generate code for calc_LR_single_no_checks_w()
  
  library(tidyverse)
  for (xD in 0L:2L) {
    for (xS in 0L:2L) {
      
      e <- d_prob_LR_w |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
      
      condition <- paste0("else if (xD == ", xD, " && xS == ", xS, ") ")
      
      if (xD == 0L && xS == 0L) {
        condition <- paste0("if (xD == ", xD, " && xS == ", xS, ") ")
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
  if (xD == 0 && xS == 0)   {
    return( (p_0*(1 - w)^4 + p_1*w^2*(1 - w)^2 + p_2*w^4)/(p_0^2*(1 - w)^4 + 2*p_0*p_1*w*(1 - w)^3 + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w^3*(1 - w) + p_2^2*w^4) )
  }
  else if (xD == 0 && xS == 1)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xD == 0 && xS == 2)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 1 && xS == 0)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xD == 1 && xS == 1)   {
    return( (4*p_0*w^2*(1 - w)^2 + p_1*w^4 + 2*p_1*w^2*(1 - w)^2 + p_1*(1 - w)^4 + 4*p_2*w^2*(1 - w)^2)/(4*p_0^2*w^2*(1 - w)^2 + 4*p_0*p_1*w^3*(1 - w) + 4*p_0*p_1*w*(1 - w)^3 + 8*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^4 + 2*p_1^2*w^2*(1 - w)^2 + p_1^2*(1 - w)^4 + 4*p_1*p_2*w^3*(1 - w) + 4*p_1*p_2*w*(1 - w)^3 + 4*p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 1 && xS == 2)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xD == 2 && xS == 0)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xD == 2 && xS == 1)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xD == 2 && xS == 2)   {
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
      
      condition <- paste0("else if (xD == ", xD, " && xS == ", xS, ") ")
      
      if (xD == 0L && xS == 0L) {
        condition <- paste0("if (xD == ", xD, " && xS == ", xS, ") ")
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
  if (xD == 0 && xS == 0)   {
    return( (p_0*(wD - 1)^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*wS^2)/(p_0^2*(wD - 1)^2*(wS - 1)^2 - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) + p_2^2*wD^2*wS^2) )
  }
  else if (xD == 0 && xS == 1)   {
    return( (2*p_0*wS*(wD - 1)^2*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wD^2*wS*(wS - 1))/(2*p_0^2*wS*(wD - 1)^2*(wS - 1) - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wD^2*wS^2 - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + 2*p_2^2*wD^2*wS*(wS - 1)) )
  }
  else if (xD == 0 && xS == 2)   {
    return( (p_0*wS^2*(wD - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*(wS - 1)^2)/(p_0^2*wS^2*(wD - 1)^2 - p_0*p_1*wD*wS^2*(wD - 1) - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + p_2^2*wD^2*(wS - 1)^2) )
  }
  else if (xD == 1 && xS == 0)   {
    return( (2*p_0*wD*(wD - 1)*(wS - 1)^2 + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*wS^2*(wD - 1))/(2*p_0^2*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*wS^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 + 2*p_2^2*wD*wS^2*(wD - 1)) )
  }
  else if (xD == 1 && xS == 1)   {
    return( (-4*p_0*wD*wS*(wD - 1)*(wS - 1) - p_1*wD^2*wS^2 - p_1*wD^2*(wS - 1)^2 - p_1*wS^2*(wD - 1)^2 - p_1*(wD - 1)^2*(wS - 1)^2 - 4*p_2*wD*wS*(wD - 1)*(wS - 1))/(-4*p_0^2*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_1*wD^2*wS*(wS - 1) + 2*p_0*p_1*wD*wS^2*(wD - 1) + 2*p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_0*p_1*wS*(wD - 1)^2*(wS - 1) - 8*p_0*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1^2*wD^2*wS^2 - p_1^2*wD^2*(wS - 1)^2 - p_1^2*wS^2*(wD - 1)^2 - p_1^2*(wD - 1)^2*(wS - 1)^2 + 2*p_1*p_2*wD^2*wS*(wS - 1) + 2*p_1*p_2*wD*wS^2*(wD - 1) + 2*p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + 2*p_1*p_2*wS*(wD - 1)^2*(wS - 1) - 4*p_2^2*wD*wS*(wD - 1)*(wS - 1)) )
  }
  else if (xD == 1 && xS == 2)   {
    return( (2*p_0*wD*wS^2*(wD - 1) + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*(wD - 1)*(wS - 1)^2)/(2*p_0^2*wD*wS^2*(wD - 1) - p_0*p_1*wD^2*wS^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wD*(wD - 1)*(wS - 1)^2) )
  }
  else if (xD == 2 && xS == 0)   {
    return( (p_0*wD^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wS^2*(wD - 1)^2)/(p_0^2*wD^2*(wS - 1)^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*wS^2*(wD - 1)^2) )
  }
  else if (xD == 2 && xS == 1)   {
    return( (2*p_0*wD^2*wS*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wS*(wD - 1)^2*(wS - 1))/(2*p_0^2*wD^2*wS*(wS - 1) - p_0*p_1*wD^2*wS^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wS*(wD - 1)^2*(wS - 1)) )
  }
  else if (xD == 2 && xS == 2)   {
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
calc_LRs_wDwS_integrate_wDwS_mc <- function(xD, xS, shape1D, shape2D, shape1S, shape2S, p, n_samples = 100) {
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
calc_LRs_wDwS_integrate_wD_mc <- function(xD, xS, shape1D, shape2D, wS, p, n_samples = 100) {
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
#' @param shape1D `wD` has beta prior (with support on 0-0.5) with parameters `shape1D` and `shape2D`
#' @param shape2D see `shape1D`
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xD`/`xS`, or vector of length 3 for reuse)
#'
#' @export
calc_LRs_wDwS_integrate_wD_num <- function(xD, xS, shape1D, shape2D, wS, p, n_samples = 100) {
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
    pi <- p[i]
    
    # Uniform prior has constant density 2 (on 0-0.5)
    f <- if (abs(shape1D - 1.0) < 1e-4 & abs(shape2D - 1.0) < 1e-4) {
      #message("Uniform/constant")
      function(wD) {
        z <- log(2) + log(calc_LRs_no_checks_wDwS(xD = xDi, xS = xSi, wD = wD, wS = wS, p = pi))
        exp(z)
      }
    } else {
      #message("dbeta05")
      function(wD) {
        z <- dbeta05(wD, shape1 = shape1D, shape2 = shape2D, log = TRUE) + 
          log(calc_LRs_no_checks_wDwS(xD = xDi, xS = xSi, wD = wD, wS = wS, p = pi))
        exp(z)
      }
    }
    
    integrate(f, lower = 0, upper = 0.5)
  })
  
  return(LRs)
}
