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
                            vector bQ_muDKDA, data matrix XQ_muDKDA,
                            vector bQ_muOppose, data matrix XQ_muOppose,
                            vector bQ_muSupport, data matrix XQ_muSupport,
                            data array[] int J_1, data vector Z_1_muDKDA_1,
                            vector r_1_muDKDA_1, data array[] int J_2,
                            data vector Z_2_muDKDA_1, vector r_2_muDKDA_1,
                            data array[] int J_3, data vector Z_3_muOppose_1,
                            vector r_3_muOppose_1, data array[] int J_4,
                            data vector Z_4_muOppose_1,
                            vector r_4_muOppose_1, data array[] int J_5,
                            data vector Z_5_muSupport_1,
                            vector r_5_muSupport_1, data array[] int J_6,
                            data vector Z_6_muSupport_1,
                            vector r_6_muSupport_1) {
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
    muDKDA += XQ_muDKDA[start : end] * bQ_muDKDA;
    muOppose += XQ_muOppose[start : end] * bQ_muOppose;
    muSupport += XQ_muSupport[start : end] * bQ_muSupport;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muDKDA[n] += r_1_muDKDA_1[J_1[nn]] * Z_1_muDKDA_1[nn]
                   + r_2_muDKDA_1[J_2[nn]] * Z_2_muDKDA_1[nn];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muOppose[n] += r_3_muOppose_1[J_3[nn]] * Z_3_muOppose_1[nn]
                     + r_4_muOppose_1[J_4[nn]] * Z_4_muOppose_1[nn];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      muSupport[n] += r_5_muSupport_1[J_5[nn]] * Z_5_muSupport_1[nn]
                      + r_6_muSupport_1[J_6[nn]] * Z_6_muSupport_1[nn];
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
  vector[N] Z_2_muDKDA_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3; // number of grouping levels
  int<lower=1> M_3; // number of coefficients per level
  array[N] int<lower=1> J_3; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_muOppose_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4; // number of grouping levels
  int<lower=1> M_4; // number of coefficients per level
  array[N] int<lower=1> J_4; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_muOppose_1;
  // data for group-level effects of ID 5
  int<lower=1> N_5; // number of grouping levels
  int<lower=1> M_5; // number of coefficients per level
  array[N] int<lower=1> J_5; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_5_muSupport_1;
  // data for group-level effects of ID 6
  int<lower=1> N_6; // number of grouping levels
  int<lower=1> M_6; // number of coefficients per level
  array[N] int<lower=1> J_6; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_6_muSupport_1;
  int prior_only; // should the likelihood be ignored?
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
  vector[K_muDKDA] bQ_muDKDA; // regression coefficients at QR scale
  vector[K_muOppose] bQ_muOppose; // regression coefficients at QR scale
  vector[K_muSupport] bQ_muSupport; // regression coefficients at QR scale
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  array[M_1] vector[N_1] z_1; // standardized group-level effects
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  array[M_2] vector[N_2] z_2; // standardized group-level effects
  vector<lower=0>[M_3] sd_3; // group-level standard deviations
  array[M_3] vector[N_3] z_3; // standardized group-level effects
  vector<lower=0>[M_4] sd_4; // group-level standard deviations
  array[M_4] vector[N_4] z_4; // standardized group-level effects
  vector<lower=0>[M_5] sd_5; // group-level standard deviations
  array[M_5] vector[N_5] z_5; // standardized group-level effects
  vector<lower=0>[M_6] sd_6; // group-level standard deviations
  array[M_6] vector[N_6] z_6; // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_muDKDA_1; // actual group-level effects
  vector[N_2] r_2_muDKDA_1; // actual group-level effects
  vector[N_3] r_3_muOppose_1; // actual group-level effects
  vector[N_4] r_4_muOppose_1; // actual group-level effects
  vector[N_5] r_5_muSupport_1; // actual group-level effects
  vector[N_6] r_6_muSupport_1; // actual group-level effects
  real lprior = 0; // prior contributions to the log posterior
  r_1_muDKDA_1 = sd_1[1] * z_1[1];
  r_2_muDKDA_1 = sd_2[1] * z_2[1];
  r_3_muOppose_1 = sd_3[1] * z_3[1];
  r_4_muOppose_1 = sd_4[1] * z_4[1];
  r_5_muSupport_1 = sd_5[1] * z_5[1];
  r_6_muSupport_1 = sd_6[1] * z_6[1];
  lprior += student_t_lpdf(bQ_muDKDA[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muDKDA[2] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[3] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[4] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[7] | 0.187247381755561, 0.559903895498748);
  lprior += normal_lpdf(bQ_muDKDA[8] | 0.3817179679375, 0.0534812588294399);
  lprior += normal_lpdf(bQ_muDKDA[9] | -0.115527096293431, 0.0746898478582107);
  lprior += normal_lpdf(bQ_muDKDA[10] | -0.264364428977438, 0.0771474348140498);
  lprior += normal_lpdf(bQ_muDKDA[11] | -0.310635679875, 0.0793458132450198);
  lprior += normal_lpdf(bQ_muDKDA[12] | -0.591063110375, 0.0864210948577346);
  lprior += normal_lpdf(bQ_muDKDA[13] | -0.7819750300625, 0.102111459152194);
  lprior += normal_lpdf(bQ_muDKDA[14] | -0.177445331620506, 0.0846542891265356);
  lprior += normal_lpdf(bQ_muDKDA[15] | -0.0688202351897994, 0.0810164555012274);
  lprior += normal_lpdf(bQ_muDKDA[16] | -0.33135462308625, 0.0951539271650947);
  lprior += normal_lpdf(bQ_muDKDA[17] | -0.420259789, 0.108438284720744);
  lprior += normal_lpdf(bQ_muDKDA[19] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[20] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[21] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[22] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[23] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[24] | -0.120683957221, 0.0885931273442402);
  lprior += normal_lpdf(bQ_muDKDA[25] | 0.526897452375, 0.11070391848471);
  lprior += normal_lpdf(bQ_muDKDA[26] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[27] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[28] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[29] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[30] | 0, 1);
  lprior += normal_lpdf(bQ_muDKDA[31] | -0.21036039525875, 0.066430239496218);
  lprior += student_t_lpdf(bQ_muOppose[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muOppose[2] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[3] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[4] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[7] | -0.481025324887113, 0.470095713244413);
  lprior += normal_lpdf(bQ_muOppose[8] | -0.0255182066839514, 0.0280212212173748);
  lprior += normal_lpdf(bQ_muOppose[9] | 0.093231171302, 0.0436864196923117);
  lprior += normal_lpdf(bQ_muOppose[10] | -0.00589146178878125, 0.0432731442808608);
  lprior += normal_lpdf(bQ_muOppose[11] | -0.108936932560675, 0.0443848380069332);
  lprior += normal_lpdf(bQ_muOppose[12] | -0.191239326695, 0.0460858846873078);
  lprior += normal_lpdf(bQ_muOppose[13] | -0.07221941885416, 0.0506447673029727);
  lprior += normal_lpdf(bQ_muOppose[14] | -0.0560966381083994, 0.0498566287197429);
  lprior += normal_lpdf(bQ_muOppose[15] | 0.00221139166980999, 0.0480931846620046);
  lprior += normal_lpdf(bQ_muOppose[16] | -0.0317907441445038, 0.0521244516011171);
  lprior += normal_lpdf(bQ_muOppose[17] | 0.037341838737085, 0.0545672340764236);
  lprior += normal_lpdf(bQ_muOppose[19] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[20] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[21] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[22] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[23] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[24] | 0.0440561147787925, 0.0458450177925281);
  lprior += normal_lpdf(bQ_muOppose[25] | -0.09997084963471, 0.0906554963524252);
  lprior += normal_lpdf(bQ_muOppose[26] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[27] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[28] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[29] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[30] | 0, 1);
  lprior += normal_lpdf(bQ_muOppose[31] | -0.2449342298125, 0.034426937287574);
  lprior += student_t_lpdf(bQ_muSupport[1] | 3.5, 0, 3);
  lprior += normal_lpdf(bQ_muSupport[2] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[3] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[4] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[7] | -0.683659640773606, 0.39318858885396);
  lprior += normal_lpdf(bQ_muSupport[8] | -0.2191949691875, 0.0238676724182843);
  lprior += normal_lpdf(bQ_muSupport[9] | -0.11059062508625, 0.0383569138133275);
  lprior += normal_lpdf(bQ_muSupport[10] | -0.0845120590263331, 0.0380732427508409);
  lprior += normal_lpdf(bQ_muSupport[11] | -0.049487419742397, 0.0388210999743934);
  lprior += normal_lpdf(bQ_muSupport[12] | 0.106259393951881, 0.039574413499867);
  lprior += normal_lpdf(bQ_muSupport[13] | 0.18934788145, 0.0427910868303192);
  lprior += normal_lpdf(bQ_muSupport[14] | -0.0332231267102306, 0.0430100666576359);
  lprior += normal_lpdf(bQ_muSupport[15] | 0.0106622106003756, 0.0413379684899538);
  lprior += normal_lpdf(bQ_muSupport[16] | -0.0132826649330269, 0.044695638495402);
  lprior += normal_lpdf(bQ_muSupport[17] | 0.0586482481953687, 0.047301028575002);
  lprior += normal_lpdf(bQ_muSupport[19] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[20] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[21] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[22] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[23] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[24] | -0.0544279870674812, 0.0377497936434503);
  lprior += normal_lpdf(bQ_muSupport[25] | -0.0625740382552744, 0.0795951894420385);
  lprior += normal_lpdf(bQ_muSupport[26] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[27] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[28] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[29] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[30] | 0, 1);
  lprior += normal_lpdf(bQ_muSupport[31] | 0.3765927425, 0.0304434522014762);
  lprior += student_t_lpdf(sd_1[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += student_t_lpdf(sd_2[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += student_t_lpdf(sd_3[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += student_t_lpdf(sd_4[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += student_t_lpdf(sd_5[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += student_t_lpdf(sd_6[1] | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, ncat, Y,
                         bQ_muDKDA, XQ_muDKDA, bQ_muOppose, XQ_muOppose,
                         bQ_muSupport, XQ_muSupport, J_1, Z_1_muDKDA_1,
                         r_1_muDKDA_1, J_2, Z_2_muDKDA_1, r_2_muDKDA_1, J_3,
                         Z_3_muOppose_1, r_3_muOppose_1, J_4, Z_4_muOppose_1,
                         r_4_muOppose_1, J_5, Z_5_muSupport_1,
                         r_5_muSupport_1, J_6, Z_6_muSupport_1,
                         r_6_muSupport_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
  target += std_normal_lpdf(z_4[1]);
  target += std_normal_lpdf(z_5[1]);
  target += std_normal_lpdf(z_6[1]);
}
generated quantities {
  // obtain the actual coefficients
  vector[K_muDKDA] b_muDKDA = XR_muDKDA_inv * bQ_muDKDA;
  // obtain the actual coefficients
  vector[K_muOppose] b_muOppose = XR_muOppose_inv * bQ_muOppose;
  // obtain the actual coefficients
  vector[K_muSupport] b_muSupport = XR_muSupport_inv * bQ_muSupport;
}

