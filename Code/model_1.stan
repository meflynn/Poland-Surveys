// generated with brms 2.18.0
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
                            data matrix XQ_muSupport, data array[] int J_1,
                            data vector Z_1_muDKDA_1, vector r_1_muDKDA_1,
                            data array[] int J_2, data vector Z_2_muOppose_1,
                            vector r_2_muOppose_1, data array[] int J_3,
                            data vector Z_3_muSupport_1,
                            vector r_3_muSupport_1) {
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
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muDKDA[n] += r_1_muDKDA_1[J_1[nn]] * Z_1_muDKDA_1[nn];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muOppose[n] += r_2_muOppose_1[J_2[nn]] * Z_2_muOppose_1[nn];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muSupport[n] += r_3_muSupport_1[J_3[nn]] * Z_3_muSupport_1[nn];
    }
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
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_muDKDA_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_muOppose_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3; // number of grouping levels
  int<lower=1> M_3; // number of coefficients per level
  array[N] int<lower=1> J_3; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_muSupport_1;
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
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  array[M_1] vector[N_1] z_1; // standardized group-level effects
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  array[M_2] vector[N_2] z_2; // standardized group-level effects
  vector<lower=0>[M_3] sd_3; // group-level standard deviations
  array[M_3] vector[N_3] z_3; // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_muDKDA_1; // actual group-level effects
  vector[N_2] r_2_muOppose_1; // actual group-level effects
  vector[N_3] r_3_muSupport_1; // actual group-level effects
  real lprior = 0; // prior contributions to the log posterior
  r_1_muDKDA_1 = sd_1[1] * z_1[1];
  r_2_muOppose_1 = sd_2[1] * z_2[1];
  r_3_muSupport_1 = sd_3[1] * z_3[1];
  lprior += normal_lpdf(bQ_muDKDA[1] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[2] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[3] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[4] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[5] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[6] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[7] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[8] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[9] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[10] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[11] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[12] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[13] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[14] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[15] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[16] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[17] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[18] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[19] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[20] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[21] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[22] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[23] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[24] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[25] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[26] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[27] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[28] | 0, 1);
  lprior += student_t_lpdf(Intercept_muDKDA | 3, 0, 2.5);
  lprior += normal_lpdf(bQ_muOppose[1] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[2] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[3] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[4] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[5] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[6] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[7] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[8] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[9] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[10] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[11] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[12] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[13] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[14] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[15] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[16] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[17] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[18] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[19] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[20] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[21] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[22] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[23] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[24] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[25] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[26] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[27] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[28] | 0, 1);
  lprior += student_t_lpdf(Intercept_muOppose | 3, 0, 2.5);
  lprior += normal_lpdf(bQ_muSupport[1] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[2] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[3] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[4] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[5] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[6] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[7] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[8] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[9] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[10] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[11] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[12] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[13] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[14] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[15] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[16] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[17] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[18] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[19] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[20] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[21] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[22] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[23] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[24] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[25] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[26] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[27] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[28] | 0, 1);
  lprior += student_t_lpdf(Intercept_muSupport | 3, 0, 2.5);
  lprior += gamma_lpdf(sd_1[1] | 1, 1);
  lprior += gamma_lpdf(sd_2[1] | 1, 1);
  lprior += gamma_lpdf(sd_3[1] | 1, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y,
                         bQ_muDKDA, Intercept_muDKDA, XQ_muDKDA, bQ_muOppose,
                         Intercept_muOppose, XQ_muOppose, bQ_muSupport,
                         Intercept_muSupport, XQ_muSupport, J_1,
                         Z_1_muDKDA_1, r_1_muDKDA_1, J_2, Z_2_muOppose_1,
                         r_2_muOppose_1, J_3, Z_3_muSupport_1,
                         r_3_muSupport_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
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

