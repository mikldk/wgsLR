if (FALSE) {
  # Generate code for calc_LR_single_no_checks_w()
  
  library(tidyverse)
  for (xd in 0L:2L) {
    for (xs in 0L:2L) {
      
      e <- d_prob_LR_w |> filter(XD_MA == xd, XS_MA == xs) |> pull(expr_chr)
      
      condition <- paste0("else if (xd == ", xd, " && xs == ", xs, ") ")
      
      if (xd == 0L && xs == 0L) {
        condition <- paste0("if (xd == ", xd, " && xs == ", xs, ") ")
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

calc_LR_single_no_checks_w <- function(xs, xd, w, p_0, p_1, p_2) {
  if (xd == 0 && xs == 0)   {
    return( (p_0*(1 - w)^4 + p_1*w^2*(1 - w)^2 + p_2*w^4)/(p_0^2*(1 - w)^4 + 2*p_0*p_1*w*(1 - w)^3 + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w^3*(1 - w) + p_2^2*w^4) )
  }
  else if (xd == 0 && xs == 1)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xd == 0 && xs == 2)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xd == 1 && xs == 0)   {
    return( (2*p_0*w*(1 - w)^3 + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w^3*(1 - w))/(2*p_0^2*w*(1 - w)^3 + 3*p_0*p_1*w^2*(1 - w)^2 + p_0*p_1*(1 - w)^4 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + p_1*p_2*w^4 + 3*p_1*p_2*w^2*(1 - w)^2 + 2*p_2^2*w^3*(1 - w)) )
  }
  else if (xd == 1 && xs == 1)   {
    return( (4*p_0*w^2*(1 - w)^2 + p_1*w^4 + 2*p_1*w^2*(1 - w)^2 + p_1*(1 - w)^4 + 4*p_2*w^2*(1 - w)^2)/(4*p_0^2*w^2*(1 - w)^2 + 4*p_0*p_1*w^3*(1 - w) + 4*p_0*p_1*w*(1 - w)^3 + 8*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^4 + 2*p_1^2*w^2*(1 - w)^2 + p_1^2*(1 - w)^4 + 4*p_1*p_2*w^3*(1 - w) + 4*p_1*p_2*w*(1 - w)^3 + 4*p_2^2*w^2*(1 - w)^2) )
  }
  else if (xd == 1 && xs == 2)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xd == 2 && xs == 0)   {
    return( (p_0*w^2*(1 - w)^2 + p_1*w^2*(1 - w)^2 + p_2*w^2*(1 - w)^2)/(p_0^2*w^2*(1 - w)^2 + p_0*p_1*w^3*(1 - w) + p_0*p_1*w*(1 - w)^3 + p_0*p_2*w^4 + p_0*p_2*(1 - w)^4 + p_1^2*w^2*(1 - w)^2 + p_1*p_2*w^3*(1 - w) + p_1*p_2*w*(1 - w)^3 + p_2^2*w^2*(1 - w)^2) )
  }
  else if (xd == 2 && xs == 1)   {
    return( (2*p_0*w^3*(1 - w) + p_1*w^3*(1 - w) + p_1*w*(1 - w)^3 + 2*p_2*w*(1 - w)^3)/(2*p_0^2*w^3*(1 - w) + p_0*p_1*w^4 + 3*p_0*p_1*w^2*(1 - w)^2 + 2*p_0*p_2*w^3*(1 - w) + 2*p_0*p_2*w*(1 - w)^3 + p_1^2*w^3*(1 - w) + p_1^2*w*(1 - w)^3 + 3*p_1*p_2*w^2*(1 - w)^2 + p_1*p_2*(1 - w)^4 + 2*p_2^2*w*(1 - w)^3) )
  }
  else if (xd == 2 && xs == 2)   {
    return( (p_0*w^4 + p_1*w^2*(1 - w)^2 + p_2*(1 - w)^4)/(p_0^2*w^4 + 2*p_0*p_1*w^3*(1 - w) + 2*p_0*p_2*w^2*(1 - w)^2 + p_1^2*w^2*(1 - w)^2 + 2*p_1*p_2*w*(1 - w)^3 + p_2^2*(1 - w)^4) )
  }
  else {
    stop("Not recognised")
  }
}

calc_LRs_no_checks_w <- function(xs, xd, w, p) {
  unlist(lapply(seq_along(xs), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_w(xs = xs[i], xd = xd[i], 
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
#' @param xs profile from suspect (of 0, 1, 2)
#' @param xd profile from case (of 0, 1, 2)
#' @param w error probability
#' @param p list of genotype probabilities (same length as `xd`/`xs`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_w <- function(xs, xd, w, p) {
  xs <- check_x(xs)
  xd <- check_x(xd)
  stopifnot(length(xs) == length(xd))
  
  check_w(w)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xs))
  check_p(p)
  stopifnot(length(p) == length(xd))
  
  LRs <- calc_LRs_no_checks_w(xs = xs, xd = xd, w = w, p = p)
  return(LRs)
}


############################################


if (FALSE) {
  # Generate code for calc_LR_single_no_checks_wDwS()
  
  library(tidyverse)
  for (xd in 0L:2L) {
    for (xs in 0L:2L) {
      
      e <- d_prob_LR_wDwS |> filter(XD_MA == xd, XS_MA == xs) |> pull(expr_chr)
      
      condition <- paste0("else if (xd == ", xd, " && xs == ", xs, ") ")
      
      if (xd == 0L && xs == 0L) {
        condition <- paste0("if (xd == ", xd, " && xs == ", xs, ") ")
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

calc_LR_single_no_checks_wDwS <- function(xs, xd, wD, wS, p_0, p_1, p_2) {
  if (xd == 0 && xs == 0)   {
    return( (p_0*(wD - 1)^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*wS^2)/(p_0^2*(wD - 1)^2*(wS - 1)^2 - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) + p_2^2*wD^2*wS^2) )
  }
  else if (xd == 0 && xs == 1)   {
    return( (2*p_0*wS*(wD - 1)^2*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wD^2*wS*(wS - 1))/(2*p_0^2*wS*(wD - 1)^2*(wS - 1) - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wD^2*wS^2 - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) + 2*p_2^2*wD^2*wS*(wS - 1)) )
  }
  else if (xd == 0 && xs == 2)   {
    return( (p_0*wS^2*(wD - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*(wS - 1)^2)/(p_0^2*wS^2*(wD - 1)^2 - p_0*p_1*wD*wS^2*(wD - 1) - p_0*p_1*wS*(wD - 1)^2*(wS - 1) + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD^2*wS*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + p_2^2*wD^2*(wS - 1)^2) )
  }
  else if (xd == 1 && xs == 0)   {
    return( (2*p_0*wD*(wD - 1)*(wS - 1)^2 + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*wS^2*(wD - 1))/(2*p_0^2*wD*(wD - 1)*(wS - 1)^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*(wD - 1)^2*(wS - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*wS^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 + 2*p_2^2*wD*wS^2*(wD - 1)) )
  }
  else if (xd == 1 && xs == 1)   {
    return( (-4*p_0*wD*wS*(wD - 1)*(wS - 1) - p_1*wD^2*wS^2 - p_1*wD^2*(wS - 1)^2 - p_1*wS^2*(wD - 1)^2 - p_1*(wD - 1)^2*(wS - 1)^2 - 4*p_2*wD*wS*(wD - 1)*(wS - 1))/(-4*p_0^2*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_1*wD^2*wS*(wS - 1) + 2*p_0*p_1*wD*wS^2*(wD - 1) + 2*p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_0*p_1*wS*(wD - 1)^2*(wS - 1) - 8*p_0*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1^2*wD^2*wS^2 - p_1^2*wD^2*(wS - 1)^2 - p_1^2*wS^2*(wD - 1)^2 - p_1^2*(wD - 1)^2*(wS - 1)^2 + 2*p_1*p_2*wD^2*wS*(wS - 1) + 2*p_1*p_2*wD*wS^2*(wD - 1) + 2*p_1*p_2*wD*(wD - 1)*(wS - 1)^2 + 2*p_1*p_2*wS*(wD - 1)^2*(wS - 1) - 4*p_2^2*wD*wS*(wD - 1)*(wS - 1)) )
  }
  else if (xd == 1 && xs == 2)   {
    return( (2*p_0*wD*wS^2*(wD - 1) + p_1*wD^2*wS*(wS - 1) + p_1*wS*(wD - 1)^2*(wS - 1) + 2*p_2*wD*(wD - 1)*(wS - 1)^2)/(2*p_0^2*wD*wS^2*(wD - 1) - p_0*p_1*wD^2*wS^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) - p_0*p_1*wS^2*(wD - 1)^2 + 2*p_0*p_2*wD*wS^2*(wD - 1) + 2*p_0*p_2*wD*(wD - 1)*(wS - 1)^2 + p_1^2*wD^2*wS*(wS - 1) + p_1^2*wS*(wD - 1)^2*(wS - 1) - p_1*p_2*wD^2*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wD*(wD - 1)*(wS - 1)^2) )
  }
  else if (xd == 2 && xs == 0)   {
    return( (p_0*wD^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wS^2*(wD - 1)^2)/(p_0^2*wD^2*(wS - 1)^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*(wD - 1)*(wS - 1)^2 + p_0*p_2*wD^2*wS^2 + p_0*p_2*(wD - 1)^2*(wS - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*wS^2*(wD - 1) - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*wS^2*(wD - 1)^2) )
  }
  else if (xd == 2 && xs == 1)   {
    return( (2*p_0*wD^2*wS*(wS - 1) + p_1*wD*wS^2*(wD - 1) + p_1*wD*(wD - 1)*(wS - 1)^2 + 2*p_2*wS*(wD - 1)^2*(wS - 1))/(2*p_0^2*wD^2*wS*(wS - 1) - p_0*p_1*wD^2*wS^2 - p_0*p_1*wD^2*(wS - 1)^2 - 2*p_0*p_1*wD*wS*(wD - 1)*(wS - 1) + 2*p_0*p_2*wD^2*wS*(wS - 1) + 2*p_0*p_2*wS*(wD - 1)^2*(wS - 1) + p_1^2*wD*wS^2*(wD - 1) + p_1^2*wD*(wD - 1)*(wS - 1)^2 - 2*p_1*p_2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wS^2*(wD - 1)^2 - p_1*p_2*(wD - 1)^2*(wS - 1)^2 + 2*p_2^2*wS*(wD - 1)^2*(wS - 1)) )
  }
  else if (xd == 2 && xs == 2)   {
    return( (p_0*wD^2*wS^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*(wD - 1)^2*(wS - 1)^2)/(p_0^2*wD^2*wS^2 - p_0*p_1*wD^2*wS*(wS - 1) - p_0*p_1*wD*wS^2*(wD - 1) + p_0*p_2*wD^2*(wS - 1)^2 + p_0*p_2*wS^2*(wD - 1)^2 + p_1^2*wD*wS*(wD - 1)*(wS - 1) - p_1*p_2*wD*(wD - 1)*(wS - 1)^2 - p_1*p_2*wS*(wD - 1)^2*(wS - 1) + p_2^2*(wD - 1)^2*(wS - 1)^2) )
  }
  else {
    stop("Not recognised")
  }
}

calc_LRs_no_checks_wDwS <- function(xs, xd, wD, wS, p) {
  unlist(lapply(seq_along(xs), function(i) {
    pi <- p[[i]]
    calc_LR_single_no_checks_wDwS(xs = xs[i], xd = xd[i], 
                                  wD = wD, 
                                  wS = wS,
                                  p_0 = pi[1L], p_1 = pi[2L], p_2 = pi[3L])
  }))
}

#' Calculate LR for a profile for one error probability, $w$
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
#' @param xs profile from suspect (of 0, 1, 2)
#' @param xd profile from case (of 0, 1, 2)
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (same length as `xd`/`xs`, or vector of length 3 for reuse)
#' 
#' @export
calc_LRs_wDwS <- function(xs, xd, wD, wS, p) {
  xs <- check_x(xs)
  xd <- check_x(xd)
  stopifnot(length(xs) == length(xd))
  
  check_w(wD)
  check_w(wS)
  
  # reuse
  p <- reuse_genotype_probs(p = p, n = length(xs))
  check_p(p)
  stopifnot(length(p) == length(xd))
  
  LRs <- calc_LRs_no_checks_wDwS(xs = xs, xd = xd, wD = wD, wS = wS, p = p)
  return(LRs)
}

