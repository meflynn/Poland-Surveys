library(tidyverse)
library(data.table)

set.seed(1234)

N_total <- 1000
N_groups <- n_distinct(provincelist$province)
N_pergroup <- floor(N_total/N_groups)

sims <- data.frame(N = rep(100, 1),
                   iters = rep(1, 1),
                   data = rep(NA, 1))

provincelist <- data.table::fread("Data/district-list.txt") |>
  group_by(province) |>
  mutate(error = round(rnorm(1, mean = 0, sd = 0.5), 2))

treatment_labels <- c("control", "t1", "t2", "t3")

tempdata <- fabricate(
  province = add_level(
    N = N_groups,
    label = unique(provincelist$province),
    error = unique(provincelist$error)
  ),
  individual = add_level(
    N = N_pergroup,
    age = round(runif(N_pergroup, 18, 85)),
    contact_pers = rbinom(N_pergroup, 1, 0.12),
    benefit_pers = rbinom(N_pergroup, 1, 0.09),
    income = sample(c(1, 2, 3, 4, 5), replace = TRUE, N_pergroup),
    gender = rbinom(N_pergroup, 1, 0.50),
    minority = rbinom(N_pergroup, 1, 0.13),
    education = round(rnorm(N_pergroup, 14.5, 3.7)),
    ideology = round(rnorm(N_pergroup, 5.8, 2.5)),
    treatment = sample(treatment_labels, replace = TRUE, N_pergroup)
  )
) |>
  mutate(control = ifelse(treatment == "control", 1, 0),
         t1 = ifelse(treatment == "t1", 1, 0),
         t2 = ifelse(treatment == "t2", 1, 0),
         t3 = ifelse(treatment == "t3", 1, 0),
         age_1 = ifelse(age >= 25 & age <=34, 1, 0),
         age_2 = ifelse(age >= 35 & age <= 44, 1, 0),
         age_3 = ifelse(age >= 45 & age <= 54, 1, 0),
         age_4 = ifelse(age >= 55 & age <= 64, 1, 0),
         age_5 = ifelse(age >= 65, 1, 0),
         income_1 = ifelse(income == 1, 1, 0),
         income_2 = ifelse(income == 2, 1, 0),
         income_3 = ifelse(income == 3, 1, 0),
         income_4 = ifelse(income == 4, 1, 0),
         xb_pos = -1.85 + -0.2 + 1 * t1 + 0.5 * t2 + 0.1 * t3 + -0.12 * age_1 + -0.09 * age_2 + -0.05 * age_3 + 0.11 * age_4 + 0.19 * age_5 + -0.03 * income_1 + 0.02 * income_2 + 0.004 * income_3 + 0.08 * income_4 + 0.52 * contact_pers + 0.04 * benefit_pers + 0.8 * gender + -0.03 * minority + 0.03 * education + 0.4 * ideology,
         xb_neg = -0.5 + -0.2 + -0.95 * t1 + -0.2 * t2 + -0.1 * t3 + 0.9 * age_1 + 0.01 * age_2 + -0.10 * age_3 + -0.18 * age_4 + -0.06 * age_5 + -0.05 * income_1 + 0.01 * income_2 + -0.03 * income_3 + 0.04 * income_4 + 0.18 * contact_pers + -0.42 * benefit_pers + 0.02 * gender + -0.01 * minority + 0.16 * education + -0.24 * ideology,
         p_pos = plogis(xb_pos),
         p_neg = plogis(xb_neg),
         y_pos = rbinom(length(p_pos), 1, prob = p_pos),
         y_neg = rbinom(length(p_neg), 1, prob = p_neg),
         response = case_when(
           y_pos == 1 ~ "Positive",
           y_neg == 1 ~ "Negative",
           y_pos == 0 & y_neg == 0 ~ "Neutral",
           y_pos == 1 & y_neg == 1 & p_pos > p_neg ~ "Positive"
         ))

