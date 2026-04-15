
  functions {
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
      if (xT[n] == 0 && xR[n] == 0)  {
          target += log(p_0sq*wRm1sq*wTm1sq - p_01*wR*wRm1*wTm1sq - p_01*wT*wRm1sq*wTm1 + p_02*wRsq*wTm1sq + p_02*wTsq*wRm1sq + p_1sq*wR*wT*wRm1*wTm1 - p_12*wRsq*wT*wTm1 - p_12*wR*wTsq*wRm1 + p_2sq*wRsq*wTsq);
        }
else if (xT[n] == 0 && xR[n] == 1)  {
          target += log(-2*p_0sq*wR*wRm1*wTm1sq + p_01*wRsq*wTm1sq + 2*p_01*wR*wT*wRm1*wTm1 + p_01*wRm1sq*wTm1sq - 2*p_02*wR*wTsq*wRm1 - 2*p_02*wR*wRm1*wTm1sq - p_1sq*wRsq*wT*wTm1 - p_1sq*wT*wRm1sq*wTm1 + p_12*wRsq*wTsq + 2*p_12*wR*wT*wRm1*wTm1 + p_12*wTsq*wRm1sq - 2*p_2sq*wR*wTsq*wRm1);
        }
else if (xT[n] == 0 && xR[n] == 2)  {
          target += log(p_0sq*wRsq*wTm1sq - p_01*wRsq*wT*wTm1 - p_01*wR*wRm1*wTm1sq + p_02*wRsq*wTsq + p_02*wRm1sq*wTm1sq + p_1sq*wR*wT*wRm1*wTm1 - p_12*wR*wTsq*wRm1 - p_12*wT*wRm1sq*wTm1 + p_2sq*wTsq*wRm1sq);
        }
else if (xT[n] == 1 && xR[n] == 0)  {
          target += log(-2*p_0sq*wT*wRm1sq*wTm1 + 2*p_01*wR*wT*wRm1*wTm1 + p_01*wTsq*wRm1sq + p_01*wRm1sq*wTm1sq - 2*p_02*wRsq*wT*wTm1 - 2*p_02*wT*wRm1sq*wTm1 - p_1sq*wR*wTsq*wRm1 - p_1sq*wR*wRm1*wTm1sq + p_12*wRsq*wTsq + p_12*wRsq*wTm1sq + 2*p_12*wR*wT*wRm1*wTm1 - 2*p_2sq*wRsq*wT*wTm1);
        }
else if (xT[n] == 1 && xR[n] == 1)  {
          target += log(4*p_0sq*wR*wT*wRm1*wTm1 - 2*p_01*wRsq*wT*wTm1 - 2*p_01*wR*wTsq*wRm1 - 2*p_01*wR*wRm1*wTm1sq - 2*p_01*wT*wRm1sq*wTm1 + 8*p_02*wR*wT*wRm1*wTm1 + p_1sq*wRsq*wTsq + p_1sq*wRsq*wTm1sq + p_1sq*wTsq*wRm1sq + p_1sq*wRm1sq*wTm1sq - 2*p_12*wRsq*wT*wTm1 - 2*p_12*wR*wTsq*wRm1 - 2*p_12*wR*wRm1*wTm1sq - 2*p_12*wT*wRm1sq*wTm1 + 4*p_2sq*wR*wT*wRm1*wTm1);
        }
else if (xT[n] == 1 && xR[n] == 2)  {
          target += log(-2*p_0sq*wRsq*wT*wTm1 + p_01*wRsq*wTsq + p_01*wRsq*wTm1sq + 2*p_01*wR*wT*wRm1*wTm1 - 2*p_02*wRsq*wT*wTm1 - 2*p_02*wT*wRm1sq*wTm1 - p_1sq*wR*wTsq*wRm1 - p_1sq*wR*wRm1*wTm1sq + 2*p_12*wR*wT*wRm1*wTm1 + p_12*wTsq*wRm1sq + p_12*wRm1sq*wTm1sq - 2*p_2sq*wT*wRm1sq*wTm1);
        }
else if (xT[n] == 2 && xR[n] == 0)  {
          target += log(p_0sq*wTsq*wRm1sq - p_01*wR*wTsq*wRm1 - p_01*wT*wRm1sq*wTm1 + p_02*wRsq*wTsq + p_02*wRm1sq*wTm1sq + p_1sq*wR*wT*wRm1*wTm1 - p_12*wRsq*wT*wTm1 - p_12*wR*wRm1*wTm1sq + p_2sq*wRsq*wTm1sq);
        }
else if (xT[n] == 2 && xR[n] == 1)  {
          target += log(-2*p_0sq*wR*wTsq*wRm1 + p_01*wRsq*wTsq + 2*p_01*wR*wT*wRm1*wTm1 + p_01*wTsq*wRm1sq - 2*p_02*wR*wTsq*wRm1 - 2*p_02*wR*wRm1*wTm1sq - p_1sq*wRsq*wT*wTm1 - p_1sq*wT*wRm1sq*wTm1 + p_12*wRsq*wTm1sq + 2*p_12*wR*wT*wRm1*wTm1 + p_12*wRm1sq*wTm1sq - 2*p_2sq*wR*wRm1*wTm1sq);
        }
else if (xT[n] == 2 && xR[n] == 2)  {
          target += log(p_0sq*wRsq*wTsq - p_01*wRsq*wT*wTm1 - p_01*wR*wTsq*wRm1 + p_02*wRsq*wTm1sq + p_02*wTsq*wRm1sq + p_1sq*wR*wT*wRm1*wTm1 - p_12*wR*wRm1*wTm1sq - p_12*wT*wRm1sq*wTm1 + p_2sq*wRm1sq*wTm1sq);
        }
 
    else {
      reject("Not recognised");
    }
  }
  
  }
  