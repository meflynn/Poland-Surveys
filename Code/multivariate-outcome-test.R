

library(targets)
library(tidyverse)
library(brms)

tar_load(survey_clean)
tar_load(priors_clean)
tar_load(model_1_province)
tar_load(model_0_bivariate)

# Make priors_clean into brms prior object

PRIORS <- get_prior(bf(mvbind(troops_100km, troops_5km) ~ treatment_group + (1 |ID| province),
                       decomp = "QR",
                       set_rescor(rescor = TRUE)),
                    data = survey_clean,
                    family = categorical(link = "logit", refcat = "Neutral")) |>
  mutate(prior = case_when(
    class == "cor" ~ "lkj(1)",
    prior == "" ~ "normal(0,2)",
    TRUE ~ prior
  ))

# Set parameters for models
CHAINS <- 4
CORES <- 4
ITERS <- 10000
WARMUP <- ITERS/2
REFRESH = 100
THIN <- 1


mvtest <- brms::brm(formula = bf(mvbind(troops_100km, troops_5km) ~ treatment_group + (1 |ID| province),
                       decomp = "QR") ,
                    family = categorical(link = "logit",
                                         refcat = "Neutral"),
                    data = survey_clean,
                    prior = PRIORS,
                    chains = CHAINS,
                    cores = CORES,
                    iter = ITERS,
                    warmup = WARMUP,
                    thin = THIN,
                    refresh = REFRESH,
                    control = list(adapt_delta = 0.90,
                                   max_treedepth = 13),
                    backend = "cmdstanr",
                      threads = threading(2))

