# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(tarchetypes) # Load other packages as needed. # nolint
library(here)
library(quarto)
library(future) # Load this here to tweak multicore/parallel processing settings
# Bayes option stuff
# Suppress brms startup messages
suppressPackageStartupMessages(library(brms))

# Set options for brms and stan
options(mc.cores = 4,
        mc.threads = 2,
        brms.backend = "cmdstanr")

set.seed(66502)

# Set target options:
tar_option_set(
  packages = c("tibble", "data.table", "brms", "sf", "tidybayes", "modelsummary", "cmdstanr",
               "marginaleffects", "flynnprojects", "viridis", "glue", "here", "kableExtra",
               "purrr", "svglite", "tarchetypes", "quarto", "future", "ggdist", "tinytex",
               "bayesplot", "patchwork"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Probably also have to expand memory for futures package using the following
# #. Memory calculated using 2000*1024^2 = 2097152000 where 2000 is number of
# MB desired
# Found here: https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
options(future.globals.maxSize= 2097152000)

# Set up future package for multicore processing.
# This uses a nested multicore setup, so you can have four cores per model and
# run four models in parallel
#plan(multiprocess)
plan(
  list(
    tweak(multisession, workers = 4),
    tweak(multisession, workers = 4)
    )
)

# Enable custom fonts
sysfonts::font_add_google("Oswald")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# This is required to help targets() identify all of the relevant script files
# and other files like PDF and image files it will have to upload.
# Used to have.
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(

  # Load raw data
  tar_target(survey_raw, "Data/raw-data/Poland Survey 2023_March 9, 2023_08.54.xlsx", format = "file"),
  tar_target(minerva_data, "Data/raw-data/opinion.data-20230221.RData", format = "file"),
  tar_target(book_prior_data, "Data/raw-data/m.c.t1.priors.csv", format = "file"), # The posterior summaries of m.c.t1 from book

  # Clean raw data
  tar_target(survey_clean, clean_survey_f(survey_raw)),
  tar_target(priors_prov_clean, clean_priors_prov_f(priordata = book_prior_data, modeldata = survey_clean)),
  tar_target(priors_dist_clean, clean_priors_dist_f(priordata = book_prior_data, modeldata = survey_clean)),

  # Load Minerva Data
  tar_target(minerva_clean, clean_minerva_f(minerva_data)),

  # Figures
  tar_target(figure_us_troops, figure_us_troops_f(survey_clean)),
  tar_target(figure_us_troops_time, figure_us_troops_compare_f(survey_clean, minerva_clean)),
  tar_target(figure_russia_relations, figure_russia_views_f(survey_clean)),

  #Balance Table
  tar_target(table_balance, table_balance_f(survey_clean)),

  # Run analysis
  tar_target(model_0_bivariate,
             model_0_bivariate_f(survey_clean, priors_prov_clean)),
  tar_target(model_1_province,
             model_1_province_f(survey_clean, priors_prov_clean)),
  tar_target(model_2_district,
             model_2_district_f(survey_clean, priors_dist_clean)),
  tar_target(model_3_full_response,
             model_3_full_response_f(survey_clean, priors_prov_clean)),
  tar_target(model_4_varying_effects,
             model_4_varying_effects_f(survey_clean, priors_prov_clean)),
  tar_target(model_5_ordered_response,
             model_5_ordered_response_f(survey_clean, priors_clean)),
  tar_target(model_6_contact,
             model_6_contact_f(survey_clean, priors_prov_clean)),
  tar_target(model_7_contact_int,
             model_7_contact_interaction_f(survey_clean, priors_prov_clean)),
  tar_target(model_8_contact_int_districts,
             model_8_contact_interaction_district_f(survey_clean, priors_dist_clean)),

  # Postestimation plots
  tar_target(model_1_figures,
             figure_province_predprob_f(modelobject = model_1_province,
                                        outcome.cats = c("Oppose",
                                                         "Support"
                                                         ),
                                        group.effects = TRUE)),
  tar_target(model_1_contrasts,
             figure_province_dist_contrasts_f(model_1_province,
                                              group.effects = TRUE)),
  tar_target(model_2_contrasts,
             figure_district_dist_contrasts_f(model_2_district,
                                              group.effects = TRUE)),
  tar_target(model_7_contrasts,
             figure_province_dist_contact_contrasts_f(model_7_contact_int,
                                                      group.effects = TRUE)),
  tar_target(model_8_contrasts,
             figure_district_dist_contact_contrasts_f(model_8_contact_int_districts,
                                                      group.effects = TRUE)),

  # Contrast Maps
  tar_target(model_1_contrast_map, figure_province_contrasts_map_f(model_1_province, group.effects = TRUE)),
  tar_target(model_2_contrast_map, figure_district_contrasts_map_f(model_2_district, group.effects = TRUE)),

  # Build appendix
  tar_quarto(poland_appendix,
             path = "Appendix/poland_appendix.qmd",
             quiet = FALSE),

  tar_quarto(website, path = ".", quiet = FALSE)

)
