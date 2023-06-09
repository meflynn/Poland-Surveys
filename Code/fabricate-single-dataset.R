library(tidyverse)
library(data.table)

set.seed(3353)
seed <- 3353

provincelist <- data.table::fread("Data/district-list.txt") |>
  group_by(province) |>
  mutate(error = round(rnorm(1, mean = 0, sd = 0.05), 3))

N_total <- 3600
N_groups <- n_distinct(provincelist$province)
N_pergroup <- N_total/N_groups


#set seed and arguments for brms models
ITER <- 2000
WARM <- ITER/2
SEED <- seed
CORES <- 16
CHAINS <- 4

sims <- data.frame(N = rep(N_total, 1),
                   iters = rep(1, 1),
                   data = rep(NA, 1))

treatment_labels <- c("control", "t1", "t2", "t3")

tempdata <- fabricate(
  province = add_level(
    N = N_groups,
    label = unique(provincelist$province),
    error_p = unique(provincelist$error_p),
    error_n = unique(provincelist$error_n)
  ),
  individual = add_level(
    N = N_pergroup
  )
) |>
  mutate(age = sample(c("Age 18-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64", "Age 65+"), replace = TRUE, N_total),
         contact_pers = rbinom(N_total, 1, 0.12),
         benefit_pers = rbinom(N_total, 1, 0.09),
         income = sample(c(1, 2, 3, 4, 5), replace = TRUE, N_total),
         gender = rbinom(N_total, 1, 0.50),
         minority = rbinom(N_total, 1, 0.13),
         education = round(rnorm(N_total, 14.5, 3.7)),
         ideology = round(rnorm(N_total, 5.8, 2.5)),
         treatment = sample(treatment_labels,
                            replace = TRUE,
                            N_total),
         control = ifelse(treatment == "control", 1, 0),
         t1 = ifelse(treatment == "t1", 1, 0),
         t2 = ifelse(treatment == "t2", 1, 0),
         t3 = ifelse(treatment == "t3", 1, 0),
         age_1 = ifelse(age == "Age 18-24", 1, 0),
         age_2 = ifelse(age == "Age 25-34", 1, 0),
         age_3 = ifelse(age == "Age 35-44", 1, 0),
         age_4 = ifelse(age == "Age 45-54", 1, 0),
         age_5 = ifelse(age == "Age 55-64", 1, 0),
         age_6 = ifelse(age == "Age 65+", 1, 0),
         age = factor(age, levels = c("Age 18-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64", "Age 65+")),
         age = relevel(age, ref = 1),
         income_1 = ifelse(income == 1, 1, 0),
         income_2 = ifelse(income == 2, 1, 0),
         income_3 = ifelse(income == 3, 1, 0),
         income_4 = ifelse(income == 4, 1, 0),
         treatment = factor(treatment),
         income = factor(income),
         xb_pos = -2.2 + error + 1 * t1 + 0.5 * t2 + 0.1 * t3 + -0.12 * age_2 + -0.09 * age_3 + -0.05 * age_4 + 0.11 * age_5 + 0.19 * age_6 + -0.03 * income_1 + 0.02 * income_2 + 0.004 * income_3 + 0.08 * income_4 + 0.52 * contact_pers + 0.04 * benefit_pers + 0.8 * gender + -0.03 * minority + 0.03 * education + 0.4 * ideology,
         xb_neg = -0.5 + error + -0.95 * t1 + -0.2 * t2 + -0.1 * t3 + 0.9 * age_2 + 0.01 * age_3 + -0.10 * age_4 + -0.18 * age_5 + -0.06 * age_6 + -0.05 * income_1 + 0.01 * income_2 + -0.03 * income_3 + 0.04 * income_4 + 0.18 * contact_pers + -0.42 * benefit_pers + 0.02 * gender + -0.01 * minority + 0.16 * education + -0.24 * ideology,
         p_pos = plogis(xb_pos),
         p_neg = plogis(xb_neg),
         y_pos = rbinom(length(p_pos), 1, prob = p_pos),
         y_neg = rbinom(length(p_neg), 1, prob = p_neg),
         response = case_when(
           y_pos == 1 ~ "Positive",
           y_neg == 1 ~ "Negative",
           y_pos == 0 & y_neg == 0 ~ "Neutral",
           y_pos == 1 & y_neg == 1 & p_pos > p_neg ~ "Positive",
           y_pos == 1 & y_neg == 1 & p_pos < p_neg ~ "Negative",
           TRUE ~ "Neutral"
         ))

tempmod <- brms::brm(response ~ treatment + contact_pers + benefit_pers + income + minority + gender + age + ideology + education +  (1 | label),
                     data = tempdata,
                     family = categorical(link = "logit",
                                          refcat = "Neutral"),
                     iter = ITER,
                     warmup = WARM,
                     seed = SEED,
                     chains = CHAINS,
                     cores = CORES,
                     backend = "cmdstanr",
                     control = list(max_treedepth = 12,
                                    adapt_delta = 0.8),
                     threads = threading(4))

summary(tempmod)
