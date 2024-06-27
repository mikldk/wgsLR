#' Re-use genotype probabilities
#' 
#' p <- c(0.2, 0.7, 0.1)
#' to012(sample_profiles_without_error(n = 2, p = p))
#' 
#' p_10loci <- reuse_genotype_probs(p, 10)
#' to012(sample_profiles_without_error(n = 2, p = p_10loci))
#' 
#' @export
reuse_genotype_probs <- function(p, n) {
  if (!is.list(p) && is.vector(p) && length(p) == 3L) {
    p <- lapply(seq_len(n), function(i) p)
  }
  
  return(p)
}

#' Allele probabilities to genotype probabilities assuming Hardy-Weinberg equilibrium
#' 
#' @examples
#' allele_probs_to_geno_probs(c(0.1, 0.2))
#' 
#' @export
allele_probs_to_geno_probs <- function(x) {
  p <- lapply(x, function(q) {
    c(q^2, 2*q*(1-q), (1-q)^2)
  })
  
  stopifnot(all(abs(unlist(lapply(p, sum)) - 1) < 1e-14))
  
  return(p)
}


#' Convert to 0/1/2 notation
#' 
#' Can both 
#' 1) Convert character genotype 0/0, 0/1, 1/0, 1/1 to 0, 1, 2 notation.
#' 2) Convert haplotypes c(0, 0), c(0, 1), c(1, 0), c(1, 1) to 0, 1, 2 notation.
#' 
#' Note that input is a list of loci, each element matrix n x 2 where 
#' n is number of individuals.
#' Output here is n x loci matrix.
#' 
#' @examples
#' to012(c("0/0", "0/1", "1/0", "1/1"))
#' 
#' Z <- sample_profiles_without_error(n = 10, p = list(
#'   c(0.25, 0.25, 0.5), c(0.01, 0.01, 0.98)))
#' Z
#' to012(Z)
#' 
#' @param x genotypes, e.g. from [sample_profiles_without_error()] 
#' 
#' @export
to012 <- function(x) {
  if (is.character(x) && is.vector(x)) {
    x <- gsub("|", "/", x, fixed = TRUE)
    x012 <- unlist(lapply(lapply(strsplit(x, "/"), as.integer), sum))
    return(x012)
  }
  
  if (!is.list(x)) {
    stop("This must be called on list of genotypes, e.g. from sample_profiles_without_error()")
  }
  #z <- lapply(x, function(y) apply(y, 1, sum))
  z <- lapply(x, function(y) rowSums(y))
  do.call(cbind, z)
}
