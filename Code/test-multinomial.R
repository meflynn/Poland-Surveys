# Multinomial test script
#

library(targets)
library(tidyverse)
library(brms)

tar_load(survey_clean)

data <- survey_clean |>
  fastDummies::dummy_cols(
    select_columns = "troops_5km"
  ) |>
  dplyr::rename(
    "Neutral" = troops_5km_Neutral,
    "Support" = troops_5km_Support,
    "Oppose" = troops_5km_Oppose,
    "DKDA" = troops_5km_DKDA
  )

data.aggregate <- data |>
  dplyr::select(treatment_group, gender, age, income, income_source, minority, education, ideology, Neutral, Support, Oppose, DKDA) |>
  group_by(treatment_group, gender, age, income, income_source, minority, education, ideology) |>
  dplyr::summarise(across(DKDA:Neutral, ~sum(.x, na.rm = TRUE))) |>
  ungroup() |>
  # Create trials count and matrix for multinomial outcome model
  mutate(trials = Neutral + Support + Oppose + DKDA,
         outcome_5km = cbind(Neutral, Support, Oppose, DKDA))


testmodel <- brms::brm(bf(outcome_5km | trials(trials) ~ 0 + Intercept + treatment_group + gender + age + income + income_source + minority + education + ideology
                          ),
                       family = multinomial(link = "logit"),
                       data = data.aggregate,
                       iter = 5000,
                       warmup = 2500,
                       chains = 4,
                       prior = set_prior(class = "b",
                                         "normal(0, 3)"),
                       thin = 1)
