#' Sample genotype without error
#'
#' @examples
#' Z <- sample_profiles_without_error(n = 2, p = c(0.25, 0.25, 0.5))
#' to012(Z)
#' Z <- sample_profiles_without_error(n = 5, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#' Z
#' to012(Z)
#' 
#' @param n number of samples
#' @param p list of genotype probabilities or vector of length 3 for single locus
#' 
#' @return list, element for each locus is a matrix with n rows and two columns
#' 
#' @export
sample_profiles_without_error <- function(n, p) {
  p <- reuse_genotype_probs(p = p, n = 1L)
  check_p(p)
  
  loci <- length(p)
  
  Z <- lapply(p, function(pi) {
    z_geno <- sample(c(0L, 1L, 2L), n, prob = pi, replace = TRUE)
    z_alleles <- matrix(0L, nrow = n, ncol = 2L)
    z_alleles[z_geno == 2L] <- 1L
    
    idx_het <- which(z_geno == 1L)
    idx_het01_idx <- runif(length(idx_het)) < 0.5
    idx_het01 <- idx_het[idx_het01_idx]
    idx_het10 <- idx_het[!idx_het01_idx]
    
    z_alleles[idx_het01, 1L] <- 1L
    z_alleles[idx_het10, 0L] <- 1L
    z_alleles
  })
  
  # Z is list of length loci, each element a matrix with n rows
  
  return(Z)
}


#' Add errors to genotypes
#'
#' @examples
#' Z <- sample_profiles_without_error(n = 5, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#' to012(Z)
#' X <- add_errors_to_genotypes(Z, 0.5)
#' X
#' to012(Z) - to012(add_errors_to_genotypes(Z, 0.0))
#' 
#' 
#' Z <- sample_profiles_without_error(n = 1000, p = c(0.5, 0.4, 0.1))
#' X1 <- add_errors_to_genotypes(Z, 0.5)
#' X2 <- add_errors_to_genotypes(Z, 0.5)
#' tab <- table(to012(X1), to012(X2))
#' tab
#' estimate_w(tab)
#' 
#' @param Z genotypes, e.g. created by [sample_profiles_without_error()]
#' @param w error probability
#' 
#' @return list, element for each locus is a matrix with n rows and two columns
#' 
#' @export
add_errors_to_genotypes <- function(Z, w) {
  if (!is.list(Z)) {
    stop("This must be called on list of genotypes, ", 
         "e.g. from sample_profiles_without_error(), not on to012()-result.")
  }
  
  loci <- length(Z)
  
  if (loci <= 0L) {
    return(Z)
  }
  
  n <- nrow(Z[[1L]])
  
  # Z is list of length loci, each element a matrix with n rows
  
  Z_errors <- lapply(seq_len(loci), function(i) {
    Zi <- Z[[i]]
    
    es <- matrix(sample(x = c(0L, 1L), 
                        size = 2L*n, 
                        prob = c(1-w, w), 
                        replace = TRUE),
                 ncol = 2)
    
    # Z   e    newZ
    # 0 + 0 -> 0
    # 0 + 1 -> 1
    # 1 + 0 -> 1
    # 1 + 1 -> 2 %% 2 -> 0
    Zinew <- (Zi + es) %% 2L
    Zinew
  })
  
  return(Z_errors)
}


#' Sample cases under Hp
#' 
#' Same latent genotype, Z, with independent errors for true donor (D) and 
#' suspect (S).
#'
#' @examples
#' sample_data_Hp(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#' sample_data_Hp(n = 10, w = 0.1, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hp(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
#' tab <- table(cases$X_D, cases$X_S)
#' tab
#' estimate_w(tab)
#'
#' @param n number of samples
#' @param w error probability
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hp <- function(n, w, p) {
  Z <- sample_profiles_without_error(n = n, p = p)
  
  X_D <- to012(add_errors_to_genotypes(Z, w = w))
  X_S <- to012(add_errors_to_genotypes(Z, w = w))

  return(list(X_D = X_D, X_S = X_S))
}

#' Sample cases under Hd
#' 
#' One latent genotype, ZD, for true donor (D) and 
#' one latent genotype, ZS, for suspect (S).
#'
#' @examples
#' sample_data_Hd(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#' sample_data_Hd(n = 10, w = 0.1, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hd(n = 1000, w = 0, p = c(0.25, 0.25, 0.5))
#' tab <- table(cases$X_D, cases$X_S)
#' tab
#'
#' @param n number of samples
#' @param w error probability
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hd <- function(n, w, p) {
  Z_D <- sample_profiles_without_error(n = n, p = p)
  X_D <- to012(add_errors_to_genotypes(Z_D, w = w))
  rm(Z_D) # to avoid using it by mistake
  
  Z_S <- sample_profiles_without_error(n = n, p = p)
  X_S <- to012(add_errors_to_genotypes(Z_S, w = w))
  
  return(list(X_D = X_D, X_S = X_S))
}

#' Add Hp-errors to table
#' 
#' @param tab table to add errors to
#' @param w error probability
#' 
#' @examples
#' Z <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z))
#' tab
#' new_tab <- add_errors_Hp(tab, w = 0.15)
#' new_tab
#' estimate_w(new_tab)
#' 
#' @export
add_errors_Hp <- function(tab, w) {
  if (is.integer(tab) && length(tab) == 3L) {
    tab <- diag(tab)
  }
  
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hp
  d_probs$prob <- unlist(lapply(d_probs$expr, function(z) eval(parse(text = z), list(w = w))))
  probs <- split(d_probs, d_probs$Z012)
  #lapply(probs, \(z) sum(z$prob))
  
  new_tab <- matrix(0L, nrow = 3L, ncol = 3L)
  for (i in seq_len(nrow(tab))) {
    #i <- 1
    num_of_each <- rmultinom(n = 1L, size = tab[i, i], prob = probs[[i]]$prob)
    
    for (j in seq_along(num_of_each)) {
      #j <- 1
      row <- probs[[i]]$XD012[j] + 1L # 012 -> 123
      col <- probs[[i]]$XS012[j] + 1L # 012 -> 123
      new_tab[row, col] <- new_tab[row, col] + num_of_each[j]
    }
  }
  
  stopifnot(isTRUE(all.equal(sum(tab), sum(new_tab))))
  
  return(new_tab)
}
 

#' Add Hp-errors to table
#' 
#' @param tab table to add errors to
#' @param w error probability
#' 
#' @examples
#' Z_D <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' Z_S <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z_D), to012(Z_S))
#' tab
#' new_tab <- add_errors_Hd(tab, w = 0.15)
#' new_tab
#' estimate_w(new_tab) # wrong!
#' Z <- sample_profiles_without_error(n = 10000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z), to012(Z))
#' tab
#' new_tab <- add_errors_Hd(tab, w = 0.25)
#' new_tab
#' estimate_w(new_tab) # ok
#' 
#' @export
add_errors_Hd <- function(tab, w) {
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hd
  d_probs$prob <- unlist(lapply(d_probs$expr, function(z) eval(parse(text = z), list(w = w))))
  probs <- lapply(split(d_probs, d_probs$ZD012), 
                  function(l) split(l, l$ZS012))
  #lapply(probs, \(l1) lapply(l1, \(l) sum(l$prob))) |> unlist()
  
  new_tab <- matrix(0L, nrow = 3L, ncol = 3L)
  for (i_row in seq_len(nrow(tab))) {
    #i_row <- 1
    for (i_col in seq_len(ncol(tab))) {
      #i_col <- 1
      d_tmp <- probs[[i_row]][[i_col]]
      num_of_each <- rmultinom(n = 1L, size = tab[i_row, i_col], 
                               prob = probs[[i_row]][[i_col]]$prob)
      
      for (j in seq_along(num_of_each)) {
        #j <- 1
        row <- d_tmp$XD012[j] + 1L # 012 -> 123
        col <- d_tmp$XS012[j] + 1L # 012 -> 123
        new_tab[row, col] <- new_tab[row, col] + num_of_each[j]
      }
    }
  }
  
  stopifnot(isTRUE(all.equal(sum(tab), sum(new_tab))))
  
  return(new_tab)
}
