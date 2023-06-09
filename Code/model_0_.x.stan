// generated with brms 2.19.0
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
    for (n in 1 : num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(array[] int seq, int start, int end,
                            data int ncat, data array[] int Y,
                            vector bQ_muDKDA, real Intercept_muDKDA,
                            data matrix XQ_muDKDA, vector bQ_muOppose,
                            real Intercept_muOppose, data matrix XQ_muOppose,
                            vector bQ_muSupport, real Intercept_muSupport,
                            data matrix XQ_muSupport) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] muDKDA = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muOppose = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muSupport = rep_vector(0.0, N);
    // linear predictor matrix
    array[N] vector[ncat] mu;
    muDKDA += Intercept_muDKDA + XQ_muDKDA[start : end] * bQ_muDKDA;
    muOppose += Intercept_muOppose + XQ_muOppose[start : end] * bQ_muOppose;
    muSupport += Intercept_muSupport
                 + XQ_muSupport[start : end] * bQ_muSupport;
    for (n in 1 : N) {
      mu[n] = transpose([muDKDA[n], 0, muOppose[n], muSupport[n]]);
    }
    for (n in 1 : N) {
      int nn = n + start - 1;
      ptarget += categorical_logit_lpmf(Y[nn] | mu[n]);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N; // total number of observations
  int<lower=2> ncat; // number of categories
  array[N] int Y; // response variable
  int<lower=1> K_muDKDA; // number of population-level effects
  matrix[N, K_muDKDA] X_muDKDA; // population-level design matrix
  int<lower=1> K_muOppose; // number of population-level effects
  matrix[N, K_muOppose] X_muOppose; // population-level design matrix
  int<lower=1> K_muSupport; // number of population-level effects
  matrix[N, K_muSupport] X_muSupport; // population-level design matrix
  int grainsize; // grainsize for threading
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  int Kc_muDKDA = K_muDKDA - 1;
  matrix[N, Kc_muDKDA] Xc_muDKDA; // centered version of X_muDKDA without an intercept
  vector[Kc_muDKDA] means_X_muDKDA; // column means of X_muDKDA before centering
  // matrices for QR decomposition
  matrix[N, Kc_muDKDA] XQ_muDKDA;
  matrix[Kc_muDKDA, Kc_muDKDA] XR_muDKDA;
  matrix[Kc_muDKDA, Kc_muDKDA] XR_muDKDA_inv;
  int Kc_muOppose = K_muOppose - 1;
  matrix[N, Kc_muOppose] Xc_muOppose; // centered version of X_muOppose without an intercept
  vector[Kc_muOppose] means_X_muOppose; // column means of X_muOppose before centering
  // matrices for QR decomposition
  matrix[N, Kc_muOppose] XQ_muOppose;
  matrix[Kc_muOppose, Kc_muOppose] XR_muOppose;
  matrix[Kc_muOppose, Kc_muOppose] XR_muOppose_inv;
  int Kc_muSupport = K_muSupport - 1;
  matrix[N, Kc_muSupport] Xc_muSupport; // centered version of X_muSupport without an intercept
  vector[Kc_muSupport] means_X_muSupport; // column means of X_muSupport before centering
  // matrices for QR decomposition
  matrix[N, Kc_muSupport] XQ_muSupport;
  matrix[Kc_muSupport, Kc_muSupport] XR_muSupport;
  matrix[Kc_muSupport, Kc_muSupport] XR_muSupport_inv;
  array[N] int seq = sequence(1, N);
  for (i in 2 : K_muDKDA) {
    means_X_muDKDA[i - 1] = mean(X_muDKDA[ : , i]);
    Xc_muDKDA[ : , i - 1] = X_muDKDA[ : , i] - means_X_muDKDA[i - 1];
  }
  // compute and scale QR decomposition
  XQ_muDKDA = qr_thin_Q(Xc_muDKDA) * sqrt(N - 1);
  XR_muDKDA = qr_thin_R(Xc_muDKDA) / sqrt(N - 1);
  XR_muDKDA_inv = inverse(XR_muDKDA);
  for (i in 2 : K_muOppose) {
    means_X_muOppose[i - 1] = mean(X_muOppose[ : , i]);
    Xc_muOppose[ : , i - 1] = X_muOppose[ : , i] - means_X_muOppose[i - 1];
  }
  // compute and scale QR decomposition
  XQ_muOppose = qr_thin_Q(Xc_muOppose) * sqrt(N - 1);
  XR_muOppose = qr_thin_R(Xc_muOppose) / sqrt(N - 1);
  XR_muOppose_inv = inverse(XR_muOppose);
  for (i in 2 : K_muSupport) {
    means_X_muSupport[i - 1] = mean(X_muSupport[ : , i]);
    Xc_muSupport[ : , i - 1] = X_muSupport[ : , i] - means_X_muSupport[i - 1];
  }
  // compute and scale QR decomposition
  XQ_muSupport = qr_thin_Q(Xc_muSupport) * sqrt(N - 1);
  XR_muSupport = qr_thin_R(Xc_muSupport) / sqrt(N - 1);
  XR_muSupport_inv = inverse(XR_muSupport);
}
parameters {
  vector[Kc_muDKDA] bQ_muDKDA; // regression coefficients at QR scale
  real Intercept_muDKDA; // temporary intercept for centered predictors
  vector[Kc_muOppose] bQ_muOppose; // regression coefficients at QR scale
  real Intercept_muOppose; // temporary intercept for centered predictors
  vector[Kc_muSupport] bQ_muSupport; // regression coefficients at QR scale
  real Intercept_muSupport; // temporary intercept for centered predictors
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  lprior += normal_lpdf(bQ_muDKDA[1] | 0, 2);
  lprior += normal_lpdf(bQ_muDKDA[2] | 0, 2);
  lprior += normal_lpdf(bQ_muDKDA[3] | 0, 2);
  lprior += student_t_lpdf(Intercept_muDKDA | 3, 0, 2.5);
  lprior += normal_lpdf(bQ_muOppose[1] | 0, 2);
  lprior += normal_lpdf(bQ_muOppose[2] | 0, 2);
  lprior += normal_lpdf(bQ_muOppose[3] | 0, 2);
  lprior += student_t_lpdf(Intercept_muOppose | 3, 0, 2.5);
  lprior += normal_lpdf(bQ_muSupport[1] | 0, 2);
  lprior += normal_lpdf(bQ_muSupport[2] | 0, 2);
  lprior += normal_lpdf(bQ_muSupport[3] | 0, 2);
  lprior += student_t_lpdf(Intercept_muSupport | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y,
                         bQ_muDKDA, Intercept_muDKDA, XQ_muDKDA, bQ_muOppose,
                         Intercept_muOppose, XQ_muOppose, bQ_muSupport,
                         Intercept_muSupport, XQ_muSupport);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // obtain the actual coefficients
  vector[Kc_muDKDA] b_muDKDA = XR_muDKDA_inv * bQ_muDKDA;
  // actual population-level intercept
  real b_muDKDA_Intercept = Intercept_muDKDA
                            - dot_product(means_X_muDKDA, b_muDKDA);
  // obtain the actual coefficients
  vector[Kc_muOppose] b_muOppose = XR_muOppose_inv * bQ_muOppose;
  // actual population-level intercept
  real b_muOppose_Intercept = Intercept_muOppose
                              - dot_product(means_X_muOppose, b_muOppose);
  // obtain the actual coefficients
  vector[Kc_muSupport] b_muSupport = XR_muSupport_inv * bQ_muSupport;
  // actual population-level intercept
  real b_muSupport_Intercept = Intercept_muSupport
                               - dot_product(means_X_muSupport, b_muSupport);
}

