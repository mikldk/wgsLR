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
    
    z_alleles[idx_het01, 2L] <- 1L
    z_alleles[idx_het10, 1L] <- 1L
    z_alleles
  })
  
  # Z is list of length loci, each element a matrix with n rows
  
  return(Z)
}

#' Beta distribution parameterisation conversion
#' 
#' Get beta distribution's two shape parameters from
#' mean value and variance.
#' 
#' @param mu mean value
#' @param sigmasq variance
#' @param a lower support bound
#' @param b upper support bound
#' 
#' @examples
#' a <- 2
#' b <- 6
#' m <- a / (a + b)
#' v <- (a*b)/((a+b)^2 * (a + b +1))
#' get_beta_parameters(m, v)
#' get_beta_parameters(1e-2, 1e-3)
#' # p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
#' # p <- get_beta_parameters(0.1, 0.001); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
#' # p <- get_beta_parameters(0.1, 0.0001); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
#' 
#' s12 <- get_beta_parameters(mu = 1e-2, sigmasq = 1e-3, a = 0, b = 0.5)
#' x <- rbeta05(1000, s12[1L], s12[2L])
#' mean(x); var(x)
#' 
#' s12 <- get_beta_parameters(mu = 1e-5, sigmasq = 1e-7, a = 0, b = 0.5)
#' x <- rbeta05(10000, s12[1L], s12[2L])
#' mean(x); var(x)
#' 
#' @export
get_beta_parameters <- function(mu, sigmasq, a = 0, b = 1) {
  # Normalise the mean and variance to standard Beta on [0,1]
  mu_std <- (mu - a) / (b - a)
  var_std <- sigmasq / ((b - a)^2)
  
  if (mu_std <= 0 || mu_std >= 1) stop("Mean must be strictly between a and b")
  if (var_std <= 0 || var_std >= mu_std * (1 - mu_std)) stop("Invalid variance for Beta distribution")
  
  # Calculate the common term
  temp <- mu_std * (1 - mu_std) / var_std - 1
  alpha <- mu_std * temp
  beta <- (1 - mu_std) * temp
  
  return(c(alpha, beta))
}

get_beta_parameters_old <- function(mu, sigmasq) {
  s1 <- ((1 - mu) / sigmasq - 1 / mu) * mu ^ 2
  s2 <- s1 * (1 / mu - 1)
  
  if (s1 <= 0 || s2 <= 0) {
    stop("s1 <= 0 || s2 <= 0: please try another (lower) variance")
  }
  
  return(c(s1, s2))
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
#' X1 <- add_errors_to_genotypes(Z, 0.1)
#' X2 <- add_errors_to_genotypes(Z, 0.1)
#' tab <- table(to012(X1), to012(X2))
#' tab
#' estimate_w(tab)
#' 
#' X1 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
#' X2 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
#' tab <- table(to012(X1), to012(X2))
#' tab
#' estimate_w(tab)
#' # p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
#' 
#' 
#' # p <- get_beta_parameters(0.1, 0.01); curve(dbeta(x, p[1], p[2]), from = 0, to = 1)
#' X1 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
#' X2 <- add_errors_to_genotypes(Z, 0.1, overdisp_var = 0.01)
#' tab <- table(to012(X1), to012(X2))
#' tab
#' estimate_w(tab)
#' 
#' @param Z genotypes, e.g. created by [sample_profiles_without_error()]
#' @param w error probability
#' @param overdisp_var if set, use as modelling overdispersion in $w$ such that $w$ 
#' follows a Beta distribution with mean value $w$ and variance `overdisp_var`
#' 
#' @return list, element for each locus is a matrix with n rows and two columns
#' 
#' @export
add_errors_to_genotypes <- function(Z, w, overdisp_var = NULL) {
  if (!is.list(Z)) {
    stop("This must be called on list of genotypes, ", 
         "e.g. from sample_profiles_without_error(), not on to012()-result.")
  }
  
  loci <- length(Z)
  
  if (loci <= 0L) {
    return(Z)
  }
  
  n <- nrow(Z[[1L]])
  
  use_overdisp <- isFALSE(is.null(overdisp_var))

  # Z is list of length loci, each element a matrix with n rows
  
  Z_errors <- lapply(seq_len(loci), function(i) {
    Zi <- Z[[i]]
    
    # es <- matrix(sample(x = c(0L, 1L), 
    #                     size = 2L*n, 
    #                     prob = c(1-w, w), 
    #                     replace = TRUE),
    #              ncol = 2)
    
    es <- if (use_overdisp) {
        overdisp_pars <- get_beta_parameters(w, overdisp_var)
        overdisp_ws <- rbeta(n = n, shape1 = overdisp_pars[1L], shape2 = overdisp_pars[2L])
        
        #message("overdispersion used")
        
        matrix(unlist(lapply(overdisp_ws, \(w_tmp) {
          sample(x = c(0L, 1L), 
                 size = 2L, 
                 prob = c(1-w_tmp, w_tmp), 
                 replace = TRUE)
        })), 
        byrow = TRUE, 
        ncol = 2L)
      } else {
        matrix(sample(x = c(0L, 1L), 
                      size = 2L*n, 
                      prob = c(1-w, w), 
                      replace = TRUE),
               ncol = 2L)
      }
    
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


#' Sample cases under Hp for one error probability, $w$
#' 
#' Same latent genotype, Z, with independent errors for true donor (D) and 
#' suspect (S).
#'
#' @examples
#' sample_data_Hp_w(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#' sample_data_Hp_w(n = 10, w = 0.1, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hp_w(n = 1000, w = 0.3, p = c(0.25, 0.25, 0.5))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' tab
#' estimate_w(tab)
#' 
#' cases <- sample_data_Hp_w(n = 1000, w = 0, p = c(0.1, 0.7, 0.2))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' diag(tab/sum(tab))
#'
#' @param n number of samples
#' @param w error probability
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#' @param \dots Passed on to [add_errors_to_genotypes()]
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hp_w <- function(n, w, p, ...) {
  Z <- sample_profiles_without_error(n = n, p = p)
  
  X_D <- to012(add_errors_to_genotypes(Z, w = w, ...))
  X_S <- to012(add_errors_to_genotypes(Z, w = w, ...))

  return(list(X_D = X_D, X_S = X_S))
}

#' Sample cases under Hp for sample-dependent error probabilities, $w_D$ and $w_S$
#' 
#' Same latent genotype, Z, with independent errors for true donor (D) and 
#' suspect (S).
#'
#' @examples
#' sample_data_Hp_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#' sample_data_Hp_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#' table(X_D = cases$X_D, X_S = cases$X_S)
#' cases <- sample_data_Hp_wDwS(n = 1000, wD = 0.1, wS = 0, p = c(0.25, 0.25, 0.5))
#' table(X_D = cases$X_D, X_S = cases$X_S)
#' cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0.1, p = c(0.25, 0.25, 0.5))
#' table(X_D = cases$X_D, X_S = cases$X_S)
#'   
#' cases <- sample_data_Hp_wDwS(n = 1000, wD = 1e-1, wS = 1e-8, p = c(0.25, 0.25, 0.5))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' tab
#' estimate_w(tab)
#' 
#' cases <- sample_data_Hp_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.1, 0.7, 0.2))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' diag(tab/sum(tab))
#'
#' @param n number of samples
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#' @param \dots Passed on to [add_errors_to_genotypes()]
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hp_wDwS <- function(n, wD, wS, p, ...) {
  Z <- sample_profiles_without_error(n = n, p = p)
  
  X_D <- to012(add_errors_to_genotypes(Z, w = wD, ...))
  X_S <- to012(add_errors_to_genotypes(Z, w = wS, ...))
  
  return(list(X_D = X_D, X_S = X_S))
}




#' Sample cases under Hd for one error probability, $w$
#' 
#' One latent genotype, ZD, for true donor (D) and 
#' one latent genotype, ZS, for suspect (S).
#'
#' @examples
#' sample_data_Hd_w(n = 10, w = 0.3, p = c(0.25, 0.25, 0.5))
#' sample_data_Hd_w(n = 10, w = 0.1, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hd_w(n = 1000, w = 0, p = c(0.25, 0.25, 0.5))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' tab
#'
#' @param n number of samples
#' @param w error probability
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#' @param \dots Passed on to [add_errors_to_genotypes()]
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hd_w <- function(n, w, p, ...) {
  Z_D <- sample_profiles_without_error(n = n, p = p)
  X_D <- to012(add_errors_to_genotypes(Z_D, w = w, ...))
  rm(Z_D) # to avoid using it by mistake
  
  Z_S <- sample_profiles_without_error(n = n, p = p)
  X_S <- to012(add_errors_to_genotypes(Z_S, w = w, ...))
  
  return(list(X_D = X_D, X_S = X_S))
}



#' Sample cases under Hd for sample-dependent error probabilities, $w_D$ and $w_S$
#' 
#' One latent genotype, ZD, for true donor (D) and 
#' one latent genotype, ZS, for suspect (S).
#'
#' @examples
#' sample_data_Hd_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = c(0.25, 0.25, 0.5))
#' sample_data_Hd_wDwS(n = 10, wD = 0.3, wS = 1e-6, p = list(
#'   c(0.25, 0.25, 0.5), c(0.1, 0.8, 0.1)))
#'   
#' cases <- sample_data_Hd_wDwS(n = 1000, wD = 1e-1, wS = 1e-8, p = c(0.25, 0.25, 0.5))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' tab
#' 
#' cases <- sample_data_Hd_wDwS(n = 1000, wD = 0, wS = 0, p = c(0.25, 0.25, 0.5))
#' tab <- table(X_D = cases$X_D, X_S = cases$X_S)
#' tab
#' 
#' @param n number of samples
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' @param p list of genotype probabilities (length is number of loci) 
#'          or vector of length 3 for single locus
#' @param \dots Passed on to [add_errors_to_genotypes()]
#'          
#' @return list of two matrices, each of size n x loci with 
#'         genotype in 0/1/2 format resembling the situation in real life.
#' @export
sample_data_Hd_wDwS <- function(n, wD, wS, p, ...) {
  Z_D <- sample_profiles_without_error(n = n, p = p)
  X_D <- to012(add_errors_to_genotypes(Z_D, w = wD, ...))
  rm(Z_D) # to avoid using it by mistake
  
  Z_S <- sample_profiles_without_error(n = n, p = p)
  X_S <- to012(add_errors_to_genotypes(Z_S, w = wS, ...))
  
  return(list(X_D = X_D, X_S = X_S))
}








#' Add Hp-errors to table for one error probability, $w$
#' 
#' @param tab table to add errors to
#' @param w error probability
#' 
#' @examples
#' Z <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z))
#' tab
#' new_tab <- add_errors_Hp_w(tab, w = 0.15)
#' new_tab
#' estimate_w(new_tab)
#' 
#' @export
add_errors_Hp_w <- function(tab, w) {
  if (is.integer(tab) && length(tab) == 3L) {
    tab <- diag(tab)
  }
  
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hp_w
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



#' Add Hp-errors to table for sample-dependent error probabilities, $w_D$ and $w_S$
#' 
#' @param tab table to add errors to
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' 
#' @examples
#' Z <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z))
#' tab
#' add_errors_Hp_wDwS(tab, wD = 0.1, wS = 1e-6)
#' add_errors_Hp_w(tab, w = 0.05)
#' 
#' @export
add_errors_Hp_wDwS <- function(tab, wD, wS) {
  if (is.integer(tab) && length(tab) == 3L) {
    tab <- diag(tab)
  }
  
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hp_wDwS
  d_probs$prob <- unlist(lapply(d_probs$expr, function(z) eval(parse(text = z), list(wD = wD, wS = wS))))
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
 

#' Add Hd-errors to table for one error probability, $w$
#' 
#' @param tab table to add errors to
#' @param w error probability
#' 
#' @examples
#' Z_D <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' Z_S <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z_D), to012(Z_S))
#' tab
#' new_tab <- add_errors_Hd_w(tab, w = 0.15)
#' new_tab
#' estimate_w(new_tab) # wrong!
#' Z <- sample_profiles_without_error(n = 10000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z), to012(Z))
#' tab
#' new_tab <- add_errors_Hp_w(tab, w = 0.25)
#' new_tab
#' estimate_w(new_tab) # ok
#' 
#' @export
add_errors_Hd_w <- function(tab, w) {
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hd_w
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



#' Add Hd-errors to table for sample-dependent error probabilities, $w_D$ and $w_S$
#' 
#' @param tab table to add errors to
#' @param wD error probability for donor sample
#' @param wS error probability for PoI sample
#' 
#' @examples
#' Z_D <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' Z_S <- sample_profiles_without_error(n = 1000, p = c(0.25, 0.25, 0.5))
#' tab <- table(to012(Z_D), to012(Z_S))
#' tab
#' add_errors_Hd_wDwS(tab, wD = 0.1, wS = 1e-6)
#' add_errors_Hd_w(tab, w = 0.05)
#' 
#' @export
add_errors_Hd_wDwS <- function(tab, wD, wS) {
  check_tab(tab)
  
  d_probs <- wgsLR::d_probtable_Hd_wDwS
  d_probs$prob <- unlist(lapply(d_probs$expr, function(z) eval(parse(text = z), list(wD = wD, wS = wS))))
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

