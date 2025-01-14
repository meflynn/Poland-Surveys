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
  real partial_log_lik_lpmf(array[] int seq, int start, int end, data int ncat, data array[] int Y, vector bQ_muDKDA, data matrix XQ_muDKDA, vector bQ_muOppose, data matrix XQ_muOppose, vector bQ_muSupport, data matrix XQ_muSupport, data array[] int J_1, data vector Z_1_muDKDA_1, data vector Z_1_muDKDA_2, data vector Z_1_muDKDA_3, data vector Z_1_muOppose_4, data vector Z_1_muOppose_5, data vector Z_1_muOppose_6, data vector Z_1_muSupport_7, data vector Z_1_muSupport_8, data vector Z_1_muSupport_9, vector r_1_muDKDA_1, vector r_1_muDKDA_2, vector r_1_muDKDA_3, vector r_1_muOppose_4, vector r_1_muOppose_5, vector r_1_muOppose_6, vector r_1_muSupport_7, vector r_1_muSupport_8, vector r_1_muSupport_9) {
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
    muDKDA += XQ_muDKDA[start:end] * bQ_muDKDA;
    muOppose += XQ_muOppose[start:end] * bQ_muOppose;
    muSupport += XQ_muSupport[start:end] * bQ_muSupport;
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muDKDA[n] += r_1_muDKDA_1[J_1[nn]] * Z_1_muDKDA_1[nn] + r_1_muDKDA_2[J_1[nn]] * Z_1_muDKDA_2[nn] + r_1_muDKDA_3[J_1[nn]] * Z_1_muDKDA_3[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muOppose[n] += r_1_muOppose_4[J_1[nn]] * Z_1_muOppose_4[nn] + r_1_muOppose_5[J_1[nn]] * Z_1_muOppose_5[nn] + r_1_muOppose_6[J_1[nn]] * Z_1_muOppose_6[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muSupport[n] += r_1_muSupport_7[J_1[nn]] * Z_1_muSupport_7[nn] + r_1_muSupport_8[J_1[nn]] * Z_1_muSupport_8[nn] + r_1_muSupport_9[J_1[nn]] * Z_1_muSupport_9[nn];
    }
    for (n in 1:N) {
      mu[n] = transpose([muDKDA[n], 0, muOppose[n], muSupport[n]]);
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
  int<lower=1> K_muDKDA;  // number of population-level effects
  matrix[N, K_muDKDA] X_muDKDA;  // population-level design matrix
  int<lower=1> K_muOppose;  // number of population-level effects
  matrix[N, K_muOppose] X_muOppose;  // population-level design matrix
  int<lower=1> K_muSupport;  // number of population-level effects
  matrix[N, K_muSupport] X_muSupport;  // population-level design matrix
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_muDKDA_1;
  vector[N] Z_1_muDKDA_2;
  vector[N] Z_1_muDKDA_3;
  vector[N] Z_1_muOppose_4;
  vector[N] Z_1_muOppose_5;
  vector[N] Z_1_muOppose_6;
  vector[N] Z_1_muSupport_7;
  vector[N] Z_1_muSupport_8;
  vector[N] Z_1_muSupport_9;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  // matrices for QR decomposition
  matrix[N, K_muDKDA] XQ_muDKDA;
  matrix[K_muDKDA, K_muDKDA] XR_muDKDA;
  matrix[K_muDKDA, K_muDKDA] XR_muDKDA_inv;
  // matrices for QR decomposition
  matrix[N, K_muOppose] XQ_muOppose;
  matrix[K_muOppose, K_muOppose] XR_muOppose;
  matrix[K_muOppose, K_muOppose] XR_muOppose_inv;
  // matrices for QR decomposition
  matrix[N, K_muSupport] XQ_muSupport;
  matrix[K_muSupport, K_muSupport] XR_muSupport;
  matrix[K_muSupport, K_muSupport] XR_muSupport_inv;
  array[N] int seq = sequence(1, N);
  // compute and scale QR decomposition
  XQ_muDKDA = qr_thin_Q(X_muDKDA) * sqrt(N - 1);
  XR_muDKDA = qr_thin_R(X_muDKDA) / sqrt(N - 1);
  XR_muDKDA_inv = inverse(XR_muDKDA);
  // compute and scale QR decomposition
  XQ_muOppose = qr_thin_Q(X_muOppose) * sqrt(N - 1);
  XR_muOppose = qr_thin_R(X_muOppose) / sqrt(N - 1);
  XR_muOppose_inv = inverse(XR_muOppose);
  // compute and scale QR decomposition
  XQ_muSupport = qr_thin_Q(X_muSupport) * sqrt(N - 1);
  XR_muSupport = qr_thin_R(X_muSupport) / sqrt(N - 1);
  XR_muSupport_inv = inverse(XR_muSupport);
}
parameters {
  vector[K_muDKDA] bQ_muDKDA;  // regression coefficients on QR scale
  vector[K_muOppose] bQ_muOppose;  // regression coefficients on QR scale
  vector[K_muSupport] bQ_muSupport;  // regression coefficients on QR scale
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_muDKDA_1;
  vector[N_1] r_1_muDKDA_2;
  vector[N_1] r_1_muDKDA_3;
  vector[N_1] r_1_muOppose_4;
  vector[N_1] r_1_muOppose_5;
  vector[N_1] r_1_muOppose_6;
  vector[N_1] r_1_muSupport_7;
  vector[N_1] r_1_muSupport_8;
  vector[N_1] r_1_muSupport_9;
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_muDKDA_1 = r_1[, 1];
  r_1_muDKDA_2 = r_1[, 2];
  r_1_muDKDA_3 = r_1[, 3];
  r_1_muOppose_4 = r_1[, 4];
  r_1_muOppose_5 = r_1[, 5];
  r_1_muOppose_6 = r_1[, 6];
  r_1_muSupport_7 = r_1[, 7];
  r_1_muSupport_8 = r_1[, 8];
  r_1_muSupport_9 = r_1[, 9];
  lprior += student_t_lpdf(bQ_muDKDA[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muDKDA[4] | 0.187247381755561, 0.559903895498748);
  lprior += normal_lpdf(bQ_muDKDA[5] | 0.3817179679375, 0.0534812588294399);
  lprior += normal_lpdf(bQ_muDKDA[6] | -0.115527096293431, 0.0746898478582107);
  lprior += normal_lpdf(bQ_muDKDA[7] | -0.264364428977438, 0.0771474348140498);
  lprior += normal_lpdf(bQ_muDKDA[8] | -0.310635679875, 0.0793458132450198);
  lprior += normal_lpdf(bQ_muDKDA[9] | -0.591063110375, 0.0864210948577346);
  lprior += normal_lpdf(bQ_muDKDA[10] | -0.7819750300625, 0.102111459152194);
  lprior += normal_lpdf(bQ_muDKDA[11] | -0.177445331620506, 0.0846542891265356);
  lprior += normal_lpdf(bQ_muDKDA[12] | -0.0688202351897994, 0.0810164555012274);
  lprior += normal_lpdf(bQ_muDKDA[13] | -0.33135462308625, 0.0951539271650947);
  lprior += normal_lpdf(bQ_muDKDA[14] | -0.420259789, 0.108438284720744);
  lprior += normal_lpdf(bQ_muDKDA[16] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[17] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[18] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[19] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[20] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[21] | -0.120683957221, 0.0885931273442402);
  lprior += normal_lpdf(bQ_muDKDA[22] | 0.526897452375, 0.11070391848471);
  lprior += normal_lpdf(bQ_muDKDA[23] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[24] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[25] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[26] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[27] | 0,1);
  lprior += normal_lpdf(bQ_muDKDA[28] | -0.21036039525875, 0.066430239496218);
  lprior += student_t_lpdf(bQ_muOppose[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muOppose[4] | -0.481025324887113, 0.470095713244413);
  lprior += normal_lpdf(bQ_muOppose[5] | -0.0255182066839514, 0.0280212212173748);
  lprior += normal_lpdf(bQ_muOppose[6] | 0.093231171302, 0.0436864196923117);
  lprior += normal_lpdf(bQ_muOppose[7] | -0.00589146178878125, 0.0432731442808608);
  lprior += normal_lpdf(bQ_muOppose[8] | -0.108936932560675, 0.0443848380069332);
  lprior += normal_lpdf(bQ_muOppose[9] | -0.191239326695, 0.0460858846873078);
  lprior += normal_lpdf(bQ_muOppose[10] | -0.07221941885416, 0.0506447673029727);
  lprior += normal_lpdf(bQ_muOppose[11] | -0.0560966381083994, 0.0498566287197429);
  lprior += normal_lpdf(bQ_muOppose[12] | 0.00221139166980999, 0.0480931846620046);
  lprior += normal_lpdf(bQ_muOppose[13] | -0.0317907441445038, 0.0521244516011171);
  lprior += normal_lpdf(bQ_muOppose[14] | 0.037341838737085, 0.0545672340764236);
  lprior += normal_lpdf(bQ_muOppose[16] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[17] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[18] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[19] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[20] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[21] | 0.0440561147787925, 0.0458450177925281);
  lprior += normal_lpdf(bQ_muOppose[22] | -0.09997084963471, 0.0906554963524252);
  lprior += normal_lpdf(bQ_muOppose[23] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[24] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[25] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[26] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[27] | 0,1);
  lprior += normal_lpdf(bQ_muOppose[28] | -0.2449342298125, 0.034426937287574);
  lprior += student_t_lpdf(bQ_muSupport[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muSupport[4] | -0.683659640773606, 0.39318858885396);
  lprior += normal_lpdf(bQ_muSupport[5] | -0.2191949691875, 0.0238676724182843);
  lprior += normal_lpdf(bQ_muSupport[6] | -0.11059062508625, 0.0383569138133275);
  lprior += normal_lpdf(bQ_muSupport[7] | -0.0845120590263331, 0.0380732427508409);
  lprior += normal_lpdf(bQ_muSupport[8] | -0.049487419742397, 0.0388210999743934);
  lprior += normal_lpdf(bQ_muSupport[9] | 0.106259393951881, 0.039574413499867);
  lprior += normal_lpdf(bQ_muSupport[10] | 0.18934788145, 0.0427910868303192);
  lprior += normal_lpdf(bQ_muSupport[11] | -0.0332231267102306, 0.0430100666576359);
  lprior += normal_lpdf(bQ_muSupport[12] | 0.0106622106003756, 0.0413379684899538);
  lprior += normal_lpdf(bQ_muSupport[13] | -0.0132826649330269, 0.044695638495402);
  lprior += normal_lpdf(bQ_muSupport[14] | 0.0586482481953687, 0.047301028575002);
  lprior += normal_lpdf(bQ_muSupport[16] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[17] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[18] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[19] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[20] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[21] | -0.0544279870674812, 0.0377497936434503);
  lprior += normal_lpdf(bQ_muSupport[22] | -0.0625740382552744, 0.0795951894420385);
  lprior += normal_lpdf(bQ_muSupport[23] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[24] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[25] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[26] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[27] | 0,1);
  lprior += normal_lpdf(bQ_muSupport[28] | 0.3765927425, 0.0304434522014762);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 9 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y, bQ_muDKDA, XQ_muDKDA, bQ_muOppose, XQ_muOppose, bQ_muSupport, XQ_muSupport, J_1, Z_1_muDKDA_1, Z_1_muDKDA_2, Z_1_muDKDA_3, Z_1_muOppose_4, Z_1_muOppose_5, Z_1_muOppose_6, Z_1_muSupport_7, Z_1_muSupport_8, Z_1_muSupport_9, r_1_muDKDA_1, r_1_muDKDA_2, r_1_muDKDA_3, r_1_muOppose_4, r_1_muOppose_5, r_1_muOppose_6, r_1_muSupport_7, r_1_muSupport_8, r_1_muSupport_9);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
}
generated quantities {
  // obtain the actual coefficients
  vector[K_muDKDA] b_muDKDA = XR_muDKDA_inv * bQ_muDKDA;
  // obtain the actual coefficients
  vector[K_muOppose] b_muOppose = XR_muOppose_inv * bQ_muOppose;
  // obtain the actual coefficients
  vector[K_muSupport] b_muSupport = XR_muSupport_inv * bQ_muSupport;
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
