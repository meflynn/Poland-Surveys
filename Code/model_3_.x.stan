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
 /* compute correlated group-level effects
  * Args:
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns:
  *   matrix of scaled group-level effects
  */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(array[] int seq, int start, int end, data int ncat, data array[] int Y, vector bQ_muStronglysupport, data matrix XQ_muStronglysupport, vector bQ_muSomewhatsupport, data matrix XQ_muSomewhatsupport, vector bQ_muSomewhatoppose, data matrix XQ_muSomewhatoppose, vector bQ_muStronglyoppose, data matrix XQ_muStronglyoppose, vector bQ_muDKDA, data matrix XQ_muDKDA, data array[] int J_1, data vector Z_1_muStronglysupport_1, data vector Z_1_muSomewhatsupport_2, data vector Z_1_muSomewhatoppose_3, data vector Z_1_muStronglyoppose_4, data vector Z_1_muDKDA_5, vector r_1_muStronglysupport_1, vector r_1_muSomewhatsupport_2, vector r_1_muSomewhatoppose_3, vector r_1_muStronglyoppose_4, vector r_1_muDKDA_5) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] muStronglysupport = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muSomewhatsupport = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muSomewhatoppose = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muStronglyoppose = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] muDKDA = rep_vector(0.0, N);
    // linear predictor matrix
    array[N] vector[ncat] mu;
    muStronglysupport += XQ_muStronglysupport[start:end] * bQ_muStronglysupport;
    muSomewhatsupport += XQ_muSomewhatsupport[start:end] * bQ_muSomewhatsupport;
    muSomewhatoppose += XQ_muSomewhatoppose[start:end] * bQ_muSomewhatoppose;
    muStronglyoppose += XQ_muStronglyoppose[start:end] * bQ_muStronglyoppose;
    muDKDA += XQ_muDKDA[start:end] * bQ_muDKDA;
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muStronglysupport[n] += r_1_muStronglysupport_1[J_1[nn]] * Z_1_muStronglysupport_1[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muSomewhatsupport[n] += r_1_muSomewhatsupport_2[J_1[nn]] * Z_1_muSomewhatsupport_2[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muSomewhatoppose[n] += r_1_muSomewhatoppose_3[J_1[nn]] * Z_1_muSomewhatoppose_3[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muStronglyoppose[n] += r_1_muStronglyoppose_4[J_1[nn]] * Z_1_muStronglyoppose_4[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muDKDA[n] += r_1_muDKDA_5[J_1[nn]] * Z_1_muDKDA_5[nn];
    }
    for (n in 1:N) {
      mu[n] = transpose([muStronglysupport[n], muSomewhatsupport[n], 0, muSomewhatoppose[n], muStronglyoppose[n], muDKDA[n]]);
    }
    for (n in 1:N) {
      int nn = n + start - 1;
      ptarget += categorical_logit_lpmf(Y[nn] | mu[n]);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  array[N] int Y;  // response variable
  int<lower=1> K_muStronglysupport;  // number of population-level effects
  matrix[N, K_muStronglysupport] X_muStronglysupport;  // population-level design matrix
  int<lower=1> K_muSomewhatsupport;  // number of population-level effects
  matrix[N, K_muSomewhatsupport] X_muSomewhatsupport;  // population-level design matrix
  int<lower=1> K_muSomewhatoppose;  // number of population-level effects
  matrix[N, K_muSomewhatoppose] X_muSomewhatoppose;  // population-level design matrix
  int<lower=1> K_muStronglyoppose;  // number of population-level effects
  matrix[N, K_muStronglyoppose] X_muStronglyoppose;  // population-level design matrix
  int<lower=1> K_muDKDA;  // number of population-level effects
  matrix[N, K_muDKDA] X_muDKDA;  // population-level design matrix
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_muStronglysupport_1;
  vector[N] Z_1_muSomewhatsupport_2;
  vector[N] Z_1_muSomewhatoppose_3;
  vector[N] Z_1_muStronglyoppose_4;
  vector[N] Z_1_muDKDA_5;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  // matrices for QR decomposition
  matrix[N, K_muStronglysupport] XQ_muStronglysupport;
  matrix[K_muStronglysupport, K_muStronglysupport] XR_muStronglysupport;
  matrix[K_muStronglysupport, K_muStronglysupport] XR_muStronglysupport_inv;
  // matrices for QR decomposition
  matrix[N, K_muSomewhatsupport] XQ_muSomewhatsupport;
  matrix[K_muSomewhatsupport, K_muSomewhatsupport] XR_muSomewhatsupport;
  matrix[K_muSomewhatsupport, K_muSomewhatsupport] XR_muSomewhatsupport_inv;
  // matrices for QR decomposition
  matrix[N, K_muSomewhatoppose] XQ_muSomewhatoppose;
  matrix[K_muSomewhatoppose, K_muSomewhatoppose] XR_muSomewhatoppose;
  matrix[K_muSomewhatoppose, K_muSomewhatoppose] XR_muSomewhatoppose_inv;
  // matrices for QR decomposition
  matrix[N, K_muStronglyoppose] XQ_muStronglyoppose;
  matrix[K_muStronglyoppose, K_muStronglyoppose] XR_muStronglyoppose;
  matrix[K_muStronglyoppose, K_muStronglyoppose] XR_muStronglyoppose_inv;
  // matrices for QR decomposition
  matrix[N, K_muDKDA] XQ_muDKDA;
  matrix[K_muDKDA, K_muDKDA] XR_muDKDA;
  matrix[K_muDKDA, K_muDKDA] XR_muDKDA_inv;
  array[N] int seq = sequence(1, N);
  // compute and scale QR decomposition
  XQ_muStronglysupport = qr_thin_Q(X_muStronglysupport) * sqrt(N - 1);
  XR_muStronglysupport = qr_thin_R(X_muStronglysupport) / sqrt(N - 1);
  XR_muStronglysupport_inv = inverse(XR_muStronglysupport);
  // compute and scale QR decomposition
  XQ_muSomewhatsupport = qr_thin_Q(X_muSomewhatsupport) * sqrt(N - 1);
  XR_muSomewhatsupport = qr_thin_R(X_muSomewhatsupport) / sqrt(N - 1);
  XR_muSomewhatsupport_inv = inverse(XR_muSomewhatsupport);
  // compute and scale QR decomposition
  XQ_muSomewhatoppose = qr_thin_Q(X_muSomewhatoppose) * sqrt(N - 1);
  XR_muSomewhatoppose = qr_thin_R(X_muSomewhatoppose) / sqrt(N - 1);
  XR_muSomewhatoppose_inv = inverse(XR_muSomewhatoppose);
  // compute and scale QR decomposition
  XQ_muStronglyoppose = qr_thin_Q(X_muStronglyoppose) * sqrt(N - 1);
  XR_muStronglyoppose = qr_thin_R(X_muStronglyoppose) / sqrt(N - 1);
  XR_muStronglyoppose_inv = inverse(XR_muStronglyoppose);
  // compute and scale QR decomposition
  XQ_muDKDA = qr_thin_Q(X_muDKDA) * sqrt(N - 1);
  XR_muDKDA = qr_thin_R(X_muDKDA) / sqrt(N - 1);
  XR_muDKDA_inv = inverse(XR_muDKDA);
}
parameters {
  vector[K_muStronglysupport] bQ_muStronglysupport;  // regression coefficients on QR scale
  vector[K_muSomewhatsupport] bQ_muSomewhatsupport;  // regression coefficients on QR scale
  vector[K_muSomewhatoppose] bQ_muSomewhatoppose;  // regression coefficients on QR scale
  vector[K_muStronglyoppose] bQ_muStronglyoppose;  // regression coefficients on QR scale
  vector[K_muDKDA] bQ_muDKDA;  // regression coefficients on QR scale
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_muStronglysupport_1;
  vector[N_1] r_1_muSomewhatsupport_2;
  vector[N_1] r_1_muSomewhatoppose_3;
  vector[N_1] r_1_muStronglyoppose_4;
  vector[N_1] r_1_muDKDA_5;
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_muStronglysupport_1 = r_1[, 1];
  r_1_muSomewhatsupport_2 = r_1[, 2];
  r_1_muSomewhatoppose_3 = r_1[, 3];
  r_1_muStronglyoppose_4 = r_1[, 4];
  r_1_muDKDA_5 = r_1[, 5];
  lprior += normal_lpdf(bQ_muStronglysupport[1] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[2] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[3] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[4] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[5] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[6] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[7] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[8] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[9] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[10] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[11] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[12] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[13] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[14] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[15] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[16] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[17] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[18] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[19] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[20] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[21] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[22] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[23] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[24] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[25] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[26] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[27] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[28] | 0,2);
  lprior += normal_lpdf(bQ_muStronglysupport[29] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[1] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[2] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[3] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[4] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[5] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[6] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[7] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[8] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[9] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[10] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[11] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[12] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[13] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[14] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[15] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[16] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[17] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[18] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[19] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[20] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[21] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[22] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[23] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[24] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[25] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[26] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[27] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[28] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatsupport[29] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[1] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[2] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[3] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[4] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[5] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[6] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[7] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[8] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[9] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[10] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[11] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[12] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[13] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[14] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[15] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[16] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[17] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[18] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[19] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[20] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[21] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[22] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[23] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[24] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[25] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[26] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[27] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[28] | 0,2);
  lprior += normal_lpdf(bQ_muSomewhatoppose[29] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[1] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[2] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[3] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[4] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[5] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[6] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[7] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[8] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[9] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[10] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[11] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[12] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[13] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[14] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[15] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[16] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[17] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[18] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[19] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[20] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[21] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[22] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[23] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[24] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[25] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[26] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[27] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[28] | 0,2);
  lprior += normal_lpdf(bQ_muStronglyoppose[29] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[1] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[2] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[3] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[4] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[5] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[6] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[7] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[8] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[9] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[10] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[11] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[12] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[13] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[14] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[15] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[16] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[17] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[18] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[19] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[20] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[21] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[22] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[23] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[24] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[25] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[26] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[27] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[28] | 0,2);
  lprior += normal_lpdf(bQ_muDKDA[29] | 0,2);
  lprior += normal_lpdf(sd_1[1] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
  lprior += normal_lpdf(sd_1[2] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
  lprior += normal_lpdf(sd_1[3] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
  lprior += normal_lpdf(sd_1[4] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
  lprior += normal_lpdf(sd_1[5] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y, bQ_muStronglysupport, XQ_muStronglysupport, bQ_muSomewhatsupport, XQ_muSomewhatsupport, bQ_muSomewhatoppose, XQ_muSomewhatoppose, bQ_muStronglyoppose, XQ_muStronglyoppose, bQ_muDKDA, XQ_muDKDA, J_1, Z_1_muStronglysupport_1, Z_1_muSomewhatsupport_2, Z_1_muSomewhatoppose_3, Z_1_muStronglyoppose_4, Z_1_muDKDA_5, r_1_muStronglysupport_1, r_1_muSomewhatsupport_2, r_1_muSomewhatoppose_3, r_1_muStronglyoppose_4, r_1_muDKDA_5);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
}
generated quantities {
  // obtain the actual coefficients
  vector[K_muStronglysupport] b_muStronglysupport = XR_muStronglysupport_inv * bQ_muStronglysupport;
  // obtain the actual coefficients
  vector[K_muSomewhatsupport] b_muSomewhatsupport = XR_muSomewhatsupport_inv * bQ_muSomewhatsupport;
  // obtain the actual coefficients
  vector[K_muSomewhatoppose] b_muSomewhatoppose = XR_muSomewhatoppose_inv * bQ_muSomewhatoppose;
  // obtain the actual coefficients
  vector[K_muStronglyoppose] b_muStronglyoppose = XR_muStronglyoppose_inv * bQ_muStronglyoppose;
  // obtain the actual coefficients
  vector[K_muDKDA] b_muDKDA = XR_muDKDA_inv * bQ_muDKDA;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
