library(tidyverse)
library(caracas)



gen_code <- function(d) {
    
  code <- "
  real wTsq  = square(wT);
  real wRsq  = square(wR);

  real wTm1  = wT - 1;
  real wRm1  = wR - 1;
  real wTm1sq = square(wTm1);
  real wRm1sq = square(wRm1);
  
  for (n in 1:N) {
    real p_0sq  = square(p_0[n]);
    real p_1sq  = square(p_1[n]);
    real p_2sq  = square(p_2[n]);
  
    real p_01  = p_0[n]*p_1[n];
    real p_02  = p_0[n]*p_2[n];
    real p_12  = p_1[n]*p_2[n];
      "
  
  simp <- function(ex) {
    ex <- gsub("p_0", "p_0[n]", ex, fixed = TRUE)
    ex <- gsub("p_1", "p_1[n]", ex, fixed = TRUE)
    ex <- gsub("p_2", "p_2[n]", ex, fixed = TRUE)
    
    ex <- gsub("p_0[n]^2", "p_0sq", ex, fixed = TRUE)
    ex <- gsub("p_1[n]^2", "p_1sq", ex, fixed = TRUE)
    ex <- gsub("p_2[n]^2", "p_2sq", ex, fixed = TRUE)
    
    ex <- gsub("(wR - 1)^2", "wRm1sq", ex, fixed = TRUE)
    ex <- gsub("(wT - 1)^2", "wTm1sq", ex, fixed = TRUE)
    
    ex <- gsub("(wR - 1)", "wRm1", ex, fixed = TRUE)
    ex <- gsub("(wT - 1)", "wTm1", ex, fixed = TRUE)
    
    ex <- gsub("p_0[n]*p_1[n]", "p_01", ex, fixed = TRUE)
    ex <- gsub("p_0[n]*p_2[n]", "p_02", ex, fixed = TRUE)
    ex <- gsub("p_1[n]*p_2[n]", "p_12", ex, fixed = TRUE)
    
    ex <- gsub("wR^2", "wRsq", ex, fixed = TRUE)
    ex <- gsub("wT^2", "wTsq", ex, fixed = TRUE)
    
    ex
  }
  
  for (xT in 0L:2L) {
    for (xR in 0L:2L) {
      #cat("xT <- ", xT, "; xR <- ", xR, "\n", sep = "")
      
      e <- d |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
      s <- caracas::as_sym(e)
      ex <- simp(s |> as_character())
      ex <- paste0("log(", ex, ")")

      condition <- paste0("else if (xT[n] == ", xT, " && xR[n] == ", xR, ") ")
      
      if (xT == 0L && xR == 0L) {
        condition <- paste0("if (xT[n] == ", xT, " && xR[n] == ", xR, ") ")
      }
      
      # code <- paste0(code, condition, " {
      #     return(", ex, ");
      #   }\n")
      
      
      code <- paste0(code, condition, " {
          target += ", ex, ";
        }\n")
    }
  }
  
  code <- paste0(code, " 
    else {
      reject(\"Not recognised\");
    }
  }
  ")

  #code
  
  allcode <- paste0("functions {
    // ------------------------------------------------------------------
    // 4-parameter Beta log-density
    // ------------------------------------------------------------------
    real beta_4p_lpdf(real y, real alpha, real beta, real lwr, real upr) {
      real x = (y - lwr) / (upr - lwr);
      return beta_lpdf(x | alpha, beta) - log(upr - lwr);
    }
  }
  
  data {
    int<lower=0> N;
    
    // Observed discrete states
    array[N] int<lower=0, upper=2> xR;
    array[N] int<lower=0, upper=2> xT;
  
    // Known quantities
    real<lower=0, upper=0.5> wR;
    array[N] real<lower=0, upper=1> p_0;
    array[N] real<lower=0, upper=1> p_1;
    array[N] real<lower=0, upper=1> p_2;
  
    // Prior hyperparameters for wT
    real<lower=0> alpha_wT;
    real<lower=0> beta_wT;
  }
  
  parameters {
    // Inference target
    real<lower=0, upper=0.5> wT;
  }
  model {
    // ------------------------------------------------------------
    // Prior
    // ------------------------------------------------------------
    target += beta_4p_lpdf(wT | alpha_wT, beta_wT, 0, 0.5);
  
    // ------------------------------------------------------------
    // Likelihood
    // ------------------------------------------------------------
    ", 
    code, 
    "
  }
  ")
  
  allcode <- gsub("\n  ", "\n", allcode, fixed = TRUE)
  
  allcode
}

code_Hp <- gen_code(d_prob_Hp_wTwR)
code_Ha <- gen_code(d_prob_Ha_wTwR)

cat(code_Hp, file = here::here("inst", "stan", "wT-posterior-distributions-Hp_autogen.stan"))
cat(code_Ha, file = here::here("inst", "stan", "wT-posterior-distributions-Ha_autogen.stan"))

