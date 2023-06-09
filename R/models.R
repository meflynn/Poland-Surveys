
suppressPackageStartupMessages(library(tidybayes))
suppressPackageStartupMessages(library(brms))


# Create function to clean up the priors for the province models
clean_priors_prov_f <- function(priordata, modeldata) {

  # Get default model priors from brms
  default.priors <- brms::get_prior(bf(troops_100k ~ 0 + Intercept + treatment_group +
                                         gender + age + income + income_source +
                                         minority + education + ideology_z + (1 | province),
                                       decomp = "QR"),
                                    family = categorical(link = "logit",
                                                         refcat = "Neutral"),
                                    data = modeldata)

    # Pull posterior estimates from book and use as priors for this paper
    # Note this only applies where  variables that overlap.
    priors.book <- read_csv(priordata) |>
      mutate(prior = glue::glue("normal({Estimate}, {Est.Error})")) |>
      dplyr::filter(grepl(".*Intercept.*|.*gender.*|.*age.*|.*income.*|.*minority.*|.*ideology.*", coef)) |>
      mutate(coef = case_when(
        grepl(".*Intercept.*", coef) ~ "Intercept",
        TRUE ~ coef
      ),
      class = "b",
      dpar = case_when(
        grepl(".*mudk.*", coef) ~ "muDKDA",
        grepl(".*muneg.*", coef) ~ "muOppose",
        grepl(".*mupos.*", coef) ~ "muSupport"
      ),
      coef = gsub("mudk_|mupos_|muneg_|", "", coef),
      coef = case_when(
        coef == "income.5.cat81M100%" ~ "income93938P",
        coef == "income.5.cat61M80%" ~ "income74063–93937",
        coef == "income.5.cat41M60%" ~ "income57188–74062",
        coef == "income.5.cat21M40%" ~ "income43340–57187",
        TRUE ~ coef
      )
      ) |>
      dplyr::filter(coef != "genderNonMbinary") |>
      dplyr::select(prior, class, coef, dpar)

    # Bind separate data frames together
    # Clean up priors data frame and insert regularizing priors for values where we don't have specific
    # priors from previous work.
    PRIORS <- empty_prior() |>
      bind_rows(priors.book, default.priors) |>
      group_by(coef, dpar, class, group) |>
      mutate(duplicate = n()) |>
      filter(prior != '' & !grepl(".*treatment_group.*|.*income_source.*", coef)
             | prior == '' & grepl(".*treatment_group.*|.*income_source.*|.*education.*", coef)
             | class == "sd"
             | coef == "Intercept") |>
      dplyr::select(-duplicate) |>
      mutate(prior = ifelse(class == "b" & prior == '', "normal(0,1)", prior),
             prior = ifelse(class == "sd", "student_t(3, 0, 3)", prior),
             prior = ifelse(class == "b" & coef == "Intercept", "student_t(3.5, 0, 3)", prior),
             lb = ifelse(class == "sd", "0", '')) |>
      ungroup() |>
      mutate(across(everything(),
                    ~ replace_na(.x, ''))) |>
      filter(dpar != '') # Removes redundant population Intercept rows

  return(PRIORS)
}



# Create function to clean up the priors for the district models
clean_priors_dist_f <- function(priordata, modeldata) {

  # Get default model priors from brms
  default.priors <- brms::get_prior(bf(troops_100k ~ 0 + Intercept + treatment_group +
                                         gender + age + income + income_source +
                                         minority + education + ideology_z + (1 | province) + (1 | province:district),
                                       decomp = "QR"),
                                    family = categorical(link = "logit",
                                                         refcat = "Neutral"),
                                    data = modeldata)

  # Pull posterior estimates from book and use as priors for this paper
  # Note this only applies where  variables that overlap.
  priors.book <- read_csv(priordata) |>
    mutate(prior = glue::glue("normal({Estimate}, {Est.Error})")) |>
    dplyr::filter(grepl(".*Intercept.*|.*gender.*|.*age.*|.*income.*|.*minority.*|.*ideology.*", coef)) |>
    mutate(coef = case_when(
      grepl(".*Intercept.*", coef) ~ "Intercept",
      TRUE ~ coef
    ),
    class = "b",
    dpar = case_when(
      grepl(".*mudk.*", coef) ~ "muDKDA",
      grepl(".*muneg.*", coef) ~ "muOppose",
      grepl(".*mupos.*", coef) ~ "muSupport"
    ),
    coef = gsub("mudk_|mupos_|muneg_|", "", coef),
    coef = case_when(
      coef == "income.5.cat81M100%" ~ "income93938P",
      coef == "income.5.cat61M80%" ~ "income74063–93937",
      coef == "income.5.cat41M60%" ~ "income57188–74062",
      coef == "income.5.cat21M40%" ~ "income43340–57187",
      TRUE ~ coef
    )
    ) |>
    dplyr::filter(coef != "genderNonMbinary") |>
    dplyr::select(prior, class, coef, dpar)

  # Bind separate data frames together
  # Clean up priors data frame and insert regularizing priors for values where we don't have specific
  # priors from previous work.
  PRIORS <- empty_prior() |>
    bind_rows(priors.book, default.priors) |>
    group_by(coef, dpar, class, group) |>
    mutate(duplicate = n()) |>
    filter(prior != '' & !grepl(".*treatment_group.*|.*income_source.*", coef)
           | prior == '' & grepl(".*treatment_group.*|.*income_source.*|.*education.*", coef)
           | class == "sd"
           | coef == "Intercept") |>
    dplyr::select(-duplicate) |>
    mutate(prior = ifelse(class == "b" & prior == '', "normal(0,1)", prior),
           prior = ifelse(class == "sd", "student_t(3, 0, 3)", prior),
           prior = ifelse(class == "b" & coef == "Intercept", "student_t(3.5, 0, 3)", prior),
           lb = ifelse(class == "sd", "0", '')) |>
    ungroup() |>
    mutate(across(everything(),
                  ~ replace_na(.x, ''))) |>
    filter(dpar != '') # Removes redundant population Intercept rows

  return(PRIORS)
}







# Modeling the treatment and outcome in a bivariate model
model_0_bivariate_f <- function(modeldata, priordata) {

  # Base model can't use priors from previous work as it only includes
  # treatment variables.
  PRIORS <- get_prior(bf(troops_100k ~ treatment_group,
                          decomp = "QR"),
                          data = modeldata,
                       family = categorical(link = "logit", refcat = "Neutral")) |>
    mutate(prior = case_when(
      prior == "" ~ "normal(0,2)",
      TRUE ~ prior
    ))

  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  # Set up right hand side predictors
  predictors_formula <- "treatment_group"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  # Generate full list of brms formula (bf()) objects to plug into model
  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.85,
                               max_treedepth = 13),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_0_.x.stan"))
  )

  return(model_out)

}


model_1_province_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata)

  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  # set up right hand side predictors
  predictors_formula <- "0 + Intercept + treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 | province)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  # Generate full list of brms formula (bf()) objects to plug into model
  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit",
                           refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 6
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 200
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~ brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.85,
                               max_treedepth = 13),
                backend = "cmdstanr",
                threads = threading(2))
  )

  return(model_out)

}



model_2_district_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata)

  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  predictors_formula <- "0 + Intercept + treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 | province) + (1 | province:district)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.85,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_2_.x.stan"))
  )

  return(model_out)

}


# Model the full categorical responses
model_3_full_response_f <- function(modeldata, priordata) {

  # Creating regularizing priors to deal with new full categorical outcome.
  PRIORS <- get_prior(bf(troops_100k_full ~ 0 + Intercept + treatment_group+ gender + age + income + income_source + minority + education + ideology_z + (1 | province),
                         decomp = "QR"),
                      data = modeldata,
                      family = categorical(link = "logit", refcat = "Neutral")) |>
    mutate(prior = case_when(
      prior == "" ~ "normal(0,2)",
      TRUE ~ prior
    ))

  # Set up list of outcome variables
  outcomes <- list("troops_100k_full", "troops_5k_full")

  predictors_formula <- "0 + Intercept + treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 | province)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.85,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_3_.x.stan"))
  )

  return(model_out)

}


# Model varying treatment effects by province.
model_4_varying_effects_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata)

  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  predictors_formula <- "0 + Intercept + treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 + treatment_group | province)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.90,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_4_.x.stan"))
  )

  return(model_out)

}


# Model varying treatment effects by province.
model_5_ordered_response_f <- function(modeldata, priordata) {

  # Prepare model data for
  modeldata.ordered <- modeldata |>
    filter(troops_100k_full != "DKDA" | troops_5k_full != "DKDA") |>
    mutate(
      across(
        .cols = c("troops_100k_full",
                  "troops_5k_full"),
        .fns = ~ factor(.x,
                        levels = c("Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat support", "Strongly support"),
                        ordered = TRUE
                        )
        )
      )

  # Creating regularizing priors to deal with new full ordered outcome.
  PRIORS <- get_prior(bf(troops_100k_full ~ treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 | province),
                         decomp = "QR"),
                      data = modeldata.ordered,
                      family = cumulative(link = "logit",
                                          threshold = "flexible")) |>
    mutate(prior = case_when(
      prior == "" ~ "normal(0,2)",
      TRUE ~ prior
    ))

  # Set up list of outcome variables
  outcomes <- list("troops_100k_full", "troops_5k_full")

  # Note: Can't remove intercept from ordered model.
  # I.e. can't use 0 + Intercept to treat it as a standard predictor.
  predictors_formula <- "treatment_group + gender + age + income + income_source + minority + education + ideology_z + (1 | province)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = cumulative(link = "logit", threshold = "flexible")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata.ordered,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.86,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_5_.x.stan"))
  )

  return(model_out)

}



# Model varying treatment effects by province.
model_6_contact_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata) |>
    filter(!grepl(".*treatment_group.*", coef)) |>
    filter(!grepl(".*province.*", group) & !grepl(".*sd.*", class))


  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  predictors_formula <- "0 + Intercept + contact_pers + gender + age + income + income_source + minority + education + ideology_z + (1 + contact_pers | treatment_group)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.92,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_6_.x.stan"))
  )

  return(model_out)

}


# Model varying treatment effects by province.
model_7_contact_interaction_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata)


  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  predictors_formula <- "0 + Intercept + treatment_group + contact_pers + treatment_group:contact_pers + gender + age + income + income_source + minority + education + ideology_z + (1 | province)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.95,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_7_.x.stan"))
  )

  return(model_out)

}




# Model varying treatment effects by province.
model_8_contact_interaction_district_f <- function(modeldata, priordata) {

  # Have to do this because the saved clean_priors object produced by targets
  # isn't seen as a brmsprior object.
  PRIORS <- empty_prior() |>
    bind_rows(priordata)


  # Set up list of outcome variables
  outcomes <- list("troops_100k", "troops_5k")

  predictors_formula <- "0 + Intercept + treatment_group + contact_pers + treatment_group:contact_pers + gender + age + income + income_source + minority + education + ideology_z + (1 | province) + (1 | province:district)"

  # Generate list of formulas
  model_form_list <- outcomes |>
    map( ~ glue::glue("{.x} ~ {predictors_formula}"))

  bayes_model_forms <- map(
    .x = model_form_list,
    .f = ~ bf(
      .x,
      decomp = "QR",
      family = categorical(link = "logit", refcat = "Neutral")
    )
  )

  # Set parameters for models
  CHAINS <- 4
  CORES <- 4
  ITERS <- 10000
  WARMUP <- ITERS/2
  REFRESH = 100
  THIN <- 1

  # Create the list object that will hold the model results
  model_out <- furrr::future_map(
    .x = seq_along(bayes_model_forms),
    .f = ~  brm(formula = bayes_model_forms[[.x]],
                data = modeldata,
                prior = PRIORS,
                chains = CHAINS,
                cores = CORES,
                iter = ITERS,
                warmup = WARMUP,
                thin = THIN,
                refresh = REFRESH,
                control = list(adapt_delta = 0.95,
                               max_treedepth = 12),
                backend = "cmdstanr",
                threads = threading(2),
                save_model = here("Code/model_8_.x.stan"))
  )

  return(model_out)

}
