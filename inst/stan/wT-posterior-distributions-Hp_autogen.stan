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
        target += log(p_0[n]*wRm1sq*wTm1sq + p_1[n]*wR*wT*wRm1*wTm1 + p_2[n]*wRsq*wTsq);
      }
else if (xT[n] == 0 && xR[n] == 1)  {
        target += log(-2*p_0[n]*wR*wRm1*wTm1sq - p_1[n]*wRsq*wT*wTm1 - p_1[n]*wT*wRm1sq*wTm1 - 2*p_2[n]*wR*wTsq*wRm1);
      }
else if (xT[n] == 0 && xR[n] == 2)  {
        target += log(p_0[n]*wRsq*wTm1sq + p_1[n]*wR*wT*wRm1*wTm1 + p_2[n]*wTsq*wRm1sq);
      }
else if (xT[n] == 1 && xR[n] == 0)  {
        target += log(-2*p_0[n]*wT*wRm1sq*wTm1 - p_1[n]*wR*wTsq*wRm1 - p_1[n]*wR*wRm1*wTm1sq - 2*p_2[n]*wRsq*wT*wTm1);
      }
else if (xT[n] == 1 && xR[n] == 1)  {
        target += log(4*p_0[n]*wR*wT*wRm1*wTm1 + p_1[n]*wRsq*wTsq + p_1[n]*wRsq*wTm1sq + p_1[n]*wTsq*wRm1sq + p_1[n]*wRm1sq*wTm1sq + 4*p_2[n]*wR*wT*wRm1*wTm1);
      }
else if (xT[n] == 1 && xR[n] == 2)  {
        target += log(-2*p_0[n]*wRsq*wT*wTm1 - p_1[n]*wR*wTsq*wRm1 - p_1[n]*wR*wRm1*wTm1sq - 2*p_2[n]*wT*wRm1sq*wTm1);
      }
else if (xT[n] == 2 && xR[n] == 0)  {
        target += log(p_0[n]*wTsq*wRm1sq + p_1[n]*wR*wT*wRm1*wTm1 + p_2[n]*wRsq*wTm1sq);
      }
else if (xT[n] == 2 && xR[n] == 1)  {
        target += log(-2*p_0[n]*wR*wTsq*wRm1 - p_1[n]*wRsq*wT*wTm1 - p_1[n]*wT*wRm1sq*wTm1 - 2*p_2[n]*wR*wRm1*wTm1sq);
      }
else if (xT[n] == 2 && xR[n] == 2)  {
        target += log(p_0[n]*wRsq*wTsq + p_1[n]*wR*wT*wRm1*wTm1 + p_2[n]*wRm1sq*wTm1sq);
      }
 
  else {
    reject("Not recognised");
  }
}

}
