// generated with brms 2.22.8
functions {
  /* integer sequence of values
   * Args:
   *   start: starting integer
   *   end: ending integer
   * Returns:
   *   an integer sequence from start to end
   */
  array[] int sequence(int start, int end) {
    array[end - start + 1] int seq;
    for (n in 1:num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(array[] int seq, int start, int end, data int ncat, data array[] int Y, data matrix Xc_muDKDA, vector b_muDKDA, real Intercept_muDKDA, data matrix Xc_muOppose, vector b_muOppose, real Intercept_muOppose, data matrix Xc_muSupport, vector b_muSupport, real Intercept_muSupport, int Kc_muDKDA) {
    real ptarget = 0;
    int N = end - start + 1;
    // joint regression coefficients over categories
    matrix[Kc_muDKDA, ncat] b;
    // joint intercepts over categories
    vector[ncat] Intercept;
    b[, 1] = b_muDKDA;
    b[, 2] = rep_vector(0, Kc_muDKDA);
    b[, 3] = b_muOppose;
    b[, 4] = b_muSupport;
    Intercept = transpose([Intercept_muDKDA, 0, Intercept_muOppose, Intercept_muSupport]);
    ptarget += categorical_logit_glm_lpmf(Y[start:end] | Xc_muDKDA[start:end], Intercept, b);
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  array[N] int Y;  // response variable
  int<lower=1> K_muDKDA;  // number of population-level effects
  matrix[N, K_muDKDA] X_muDKDA;  // population-level design matrix
  int<lower=1> Kc_muDKDA;  // number of population-level effects after centering
  int<lower=1> K_muOppose;  // number of population-level effects
  matrix[N, K_muOppose] X_muOppose;  // population-level design matrix
  int<lower=1> Kc_muOppose;  // number of population-level effects after centering
  int<lower=1> K_muSupport;  // number of population-level effects
  matrix[N, K_muSupport] X_muSupport;  // population-level design matrix
  int<lower=1> Kc_muSupport;  // number of population-level effects after centering
  int grainsize;  // grainsize for threading
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc_muDKDA] Xc_muDKDA;  // centered version of X_muDKDA without an intercept
  vector[Kc_muDKDA] means_X_muDKDA;  // column means of X_muDKDA before centering
  matrix[N, Kc_muOppose] Xc_muOppose;  // centered version of X_muOppose without an intercept
  vector[Kc_muOppose] means_X_muOppose;  // column means of X_muOppose before centering
  matrix[N, Kc_muSupport] Xc_muSupport;  // centered version of X_muSupport without an intercept
  vector[Kc_muSupport] means_X_muSupport;  // column means of X_muSupport before centering
  array[N] int seq = sequence(1, N);
  for (i in 2:K_muDKDA) {
    means_X_muDKDA[i - 1] = mean(X_muDKDA[, i]);
    Xc_muDKDA[, i - 1] = X_muDKDA[, i] - means_X_muDKDA[i - 1];
  }
  for (i in 2:K_muOppose) {
    means_X_muOppose[i - 1] = mean(X_muOppose[, i]);
    Xc_muOppose[, i - 1] = X_muOppose[, i] - means_X_muOppose[i - 1];
  }
  for (i in 2:K_muSupport) {
    means_X_muSupport[i - 1] = mean(X_muSupport[, i]);
    Xc_muSupport[, i - 1] = X_muSupport[, i] - means_X_muSupport[i - 1];
  }
}
parameters {
  vector[Kc_muDKDA] b_muDKDA;  // regression coefficients
  real Intercept_muDKDA;  // temporary intercept for centered predictors
  vector[Kc_muOppose] b_muOppose;  // regression coefficients
  real Intercept_muOppose;  // temporary intercept for centered predictors
  vector[Kc_muSupport] b_muSupport;  // regression coefficients
  real Intercept_muSupport;  // temporary intercept for centered predictors
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b_muDKDA[1] | 0,2);
  lprior += normal_lpdf(b_muDKDA[2] | 0,2);
  lprior += normal_lpdf(b_muDKDA[3] | 0,2);
  lprior += student_t_lpdf(Intercept_muDKDA | 3, 0, 2.5);
  lprior += normal_lpdf(b_muOppose[1] | 0,2);
  lprior += normal_lpdf(b_muOppose[2] | 0,2);
  lprior += normal_lpdf(b_muOppose[3] | 0,2);
  lprior += student_t_lpdf(Intercept_muOppose | 3, 0, 2.5);
  lprior += normal_lpdf(b_muSupport[1] | 0,2);
  lprior += normal_lpdf(b_muSupport[2] | 0,2);
  lprior += normal_lpdf(b_muSupport[3] | 0,2);
  lprior += student_t_lpdf(Intercept_muSupport | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y, Xc_muDKDA, b_muDKDA, Intercept_muDKDA, Xc_muOppose, b_muOppose, Intercept_muOppose, Xc_muSupport, b_muSupport, Intercept_muSupport, Kc_muDKDA);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_muDKDA_Intercept = Intercept_muDKDA - dot_product(means_X_muDKDA, b_muDKDA);
  // actual population-level intercept
  real b_muOppose_Intercept = Intercept_muOppose - dot_product(means_X_muOppose, b_muOppose);
  // actual population-level intercept
  real b_muSupport_Intercept = Intercept_muSupport - dot_product(means_X_muSupport, b_muSupport);
}
