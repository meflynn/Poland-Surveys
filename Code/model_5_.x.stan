// generated with brms 2.22.8
functions {
  /* cumulative-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     if (y == 1) {
       return log_inv_logit(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       return log1m_inv_logit(disc * (thres[nthres] - mu));
     } else {
       return log_inv_logit_diff(disc * (thres[y] - mu), disc * (thres[y - 1] - mu));
     }
   }
  /* cumulative-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, array[] int j) {
     return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
  /* ordered-logistic log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real ordered_logistic_merged_lpmf(int y, real mu, vector thres, array[] int j) {
     return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
   }

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
  real partial_log_lik_lpmf(array[] int seq, int start, int end, data array[] int Y, data int nthres, vector bQ, data matrix XQ, vector Intercept, real disc, data array[] int J_1, data vector Z_1_1, vector r_1_1) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += XQ[start:end] * bQ;
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mu[n] += r_1_1[J_1[nn]] * Z_1_1[nn];
    }
    for (n in 1:N) {
      int nn = n + start - 1;
      ptarget += ordered_logistic_lpmf(Y[nn] | mu[n], Intercept);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X
  vector[Kc] means_X;  // column means of X before centering
  // matrices for QR decomposition
  matrix[N, Kc] XQ;
  matrix[Kc, Kc] XR;
  matrix[Kc, Kc] XR_inv;
  array[N] int seq = sequence(1, N);
  for (i in 1:K) {
    means_X[i] = mean(X[, i]);
    Xc[, i] = X[, i] - means_X[i];
  }
  // compute and scale QR decomposition
  XQ = qr_thin_Q(Xc) * sqrt(N - 1);
  XR = qr_thin_R(Xc) / sqrt(N - 1);
  XR_inv = inverse(XR);
}
parameters {
  vector[Kc] bQ;  // regression coefficients on QR scale
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
}
transformed parameters {
  real disc = 1;  // discrimination parameters
  vector[N_1] r_1_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += normal_lpdf(bQ[1] | 0,2);
  lprior += normal_lpdf(bQ[2] | 0,2);
  lprior += normal_lpdf(bQ[3] | 0,2);
  lprior += normal_lpdf(bQ[4] | 0,2);
  lprior += normal_lpdf(bQ[5] | 0,2);
  lprior += normal_lpdf(bQ[6] | 0,2);
  lprior += normal_lpdf(bQ[7] | 0,2);
  lprior += normal_lpdf(bQ[8] | 0,2);
  lprior += normal_lpdf(bQ[9] | 0,2);
  lprior += normal_lpdf(bQ[10] | 0,2);
  lprior += normal_lpdf(bQ[11] | 0,2);
  lprior += normal_lpdf(bQ[12] | 0,2);
  lprior += normal_lpdf(bQ[13] | 0,2);
  lprior += normal_lpdf(bQ[14] | 0,2);
  lprior += normal_lpdf(bQ[15] | 0,2);
  lprior += normal_lpdf(bQ[16] | 0,2);
  lprior += normal_lpdf(bQ[17] | 0,2);
  lprior += normal_lpdf(bQ[18] | 0,2);
  lprior += normal_lpdf(bQ[19] | 0,2);
  lprior += normal_lpdf(bQ[20] | 0,2);
  lprior += normal_lpdf(bQ[21] | 0,2);
  lprior += normal_lpdf(bQ[22] | 0,2);
  lprior += normal_lpdf(bQ[23] | 0,2);
  lprior += normal_lpdf(bQ[24] | 0,2);
  lprior += normal_lpdf(bQ[25] | 0,2);
  lprior += normal_lpdf(bQ[26] | 0,2);
  lprior += normal_lpdf(bQ[27] | 0,2);
  lprior += normal_lpdf(bQ[28] | 0,2);
  lprior += normal_lpdf(Intercept[1] | 0,2);
  lprior += normal_lpdf(Intercept[2] | 0,2);
  lprior += normal_lpdf(Intercept[3] | 0,2);
  lprior += normal_lpdf(Intercept[4] | 0,2);
  lprior += normal_lpdf(sd_1[1] | 0,2)
    - 1 * normal_lccdf(0 | 0,2);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, nthres, bQ, XQ, Intercept, disc, J_1, Z_1_1, r_1_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // obtain the actual coefficients
  vector[Kc] b = XR_inv * bQ;
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
}
