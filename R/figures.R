

sysfonts::font_add_google("Oswald", family = "oswald")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# Views of US
figure_us_troops_f <- function(data) {

  ggplot(data = data, aes(x = american_troops, y = after_stat(count/sum(count)))) +
    geom_bar(fill = "dodgerblue1", color = "black", linewidth = 0.1) +
    theme_flynn(base_family = "Oswald", base_size = 11) +
    scale_y_continuous(labels = scales::percent_format(),
                       breaks = seq(0, 0.5, 0.1),
                       limits = c(0, 0.5),
                       expand = c(0, 0)) +
    theme(axis.ticks.x.bottom = element_blank()) +
    labs(x = "",
         y = "Percent",
         title = "Views of U.S. Military Personnel Stationed in Poland, 2023")

  ggsave(here("Figures/views-us-troops.png"), dpi = 300, width = 5, height = 3)

}


# Views of Russia
figure_russia_views_f <- function(data) {

  ggplot(data = data, aes(x = relations_russia, y = after_stat(count/sum(count)))) +
    geom_bar(fill = "dodgerblue1", color = "black", linewidth = 0.1) +
    theme_flynn(base_family = "Oswald", base_size = 11) +
    scale_y_continuous(labels = scales::percent_format(),
                       breaks = seq(0, 0.5, 0.1),
                       limits = c(0, 0.5),
                       expand = c(0, 0)) +
    theme(axis.ticks.x.bottom = element_blank()) +
    labs(x = "",
         y = "Percent",
         title = "Views of Polish-Russian Relations, 2023")

  ggsave(here("Figures/views-russian-relations.png"), dpi = 300, width = 5, height = 3)

}


# Views of US over time
figure_us_troops_compare_f <- function(data1, data2) {

  data1 <- data1 |>
    dplyr::select(american_troops) |>
    mutate(year = 2023) |>
    filter(!is.na(american_troops))

  data2 <- data2 |>
    filter(country == "Poland") |>
    dplyr::select(year, troops_1) |>
    dplyr::rename(american_troops = troops_1) |>
    filter(!is.na(american_troops))

  data.combined <- bind_rows(data1, data2) |>
    mutate(american_troops = ifelse(grepl(".*know.*", as.character(american_troops)), "Don't know/Decline to answer", as.character(american_troops))) |>
    group_by(year, american_troops) |>
    dplyr::summarise(obs = n()) |>
    mutate(year = factor(year,
                         levels = c("2023", "2020", "2019", "2018")),
           american_troops = factor(american_troops,
           levels = c("Don't know/Decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")))

  ggplot(data.combined, aes(y = year, fill = american_troops, x = obs)) +
    geom_bar(stat = "identity", position = "fill", linewidth = 0.15, color = "black") +
    theme_flynn(base_family = "Oswald", base_size = 11) +
    theme(plot.title.position = "plot",
          plot.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.left = element_text(face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    viridis::scale_fill_viridis(option = "turbo",
                                discrete = TRUE,
                                begin = 0,
                                end = 1.0,
                                direction = -1) +
    labs(x = "",
         y = "",
         fill = "Expressed Attitude",
         title = "Polish Adults' Views of U.S. Military Personnel in Poland")

  ggsave(here("Figures/views-us-troops-time.png"), dpi = 300, width = 7, height = 3)

}



# Post estimation figures


# Substantive Effects of basic province model

figure_province_predprob_f <- function(modelobject, outcome.cats, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , str_extract(as.character(modelobject[[1]]$formula[1]), pattern = "\\([^()]+\\)")),
                     quote = FALSE)

    } else {

      re_form <- NA

      }

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              province = unique(modelobject[[1]]$data$province),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z))

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  plotlist <- map(.x = seq_along(modelobject),
                  .f = ~ add_epred_draws(modelobject[[.x]],
                                         newdata = newdataframe,
                                         ndraws = 500,
                                         .value = ".epred",
                                         re_formula = re_form

        ) |>
          mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"), # create identifier for outcome and model
                 model = str_extract(model, "\\d+k"))
        ) |> # plot the results of the model objects
        furrr::future_map(.f = ~ ggplot(data = .x |> filter(.category %in% outcome.cats), # Choose which outcome categories to include in plot
                                        aes(x = .epred)) +
                            facet_wrap(~ .category,
                                       ncol = 1) +
                            ggdist::stat_dotsinterval(aes(fill = treatment_group),
                                                      scale = 0.9,
                                                      overflow = "compress",
                                                      quantiles = 100,
                                                      .width = c(0.50, 0.89),
                                                      position = position_dodge(width = 1.2),
                                                      slab_linewidth = 0,
                                                      point_size = 0,
                                                      interval_color = NA,
                                                      point_color = NA) +
                            ggdist::stat_pointinterval(aes(group = treatment_group),
                                                       scale = 0.9,
                                                      .width = c(0.50, 0.89),
                                                      position = position_dodge(width = 1.2),
                                                      slab_linewidth = 0,
                                                      point_size = 2,
                                                      interval_color = "black",
                                                      point_color = "black") +
                            theme_flynn(base_family = "oswald") +
                            theme(axis.text.y.left = element_blank(),
                                  axis.ticks.y.left = element_blank()) +
                            scale_x_continuous(limits = c(0.0, 1.0, 0.1)) +
                            viridis::scale_fill_viridis(discrete = TRUE,
                                                        option = "turbo",
                                                        begin = 0.0,
                                                        end = 1.0) +
                            guides(fill = guide_legend(reverse = TRUE)) +
                            labs(x = "Predicted Probability",
                                 y = "",
                                 fill = "Treatment Group",
                                 title = glue::glue("{.x$model}"))
                          )
  # Generate list of plot names
  plotnames <- furrr::future_map(.x = plotlist,
                                 .f = ~ glue::glue("predicted-prob-{unique(.x$data$model)}.png"))

  # Save the plots while applying the plot names
  pwalk(list(plotnames, plotlist),
        ggsave,
        height = 5,
        width = 8,
        units = "in",
        path = here::here("Figures/"))

  # Generate combined plot.
  patchwork::wrap_plots(plotlist[[1]], plotlist[[2]]) +
    patchwork::plot_layout(guides = "collect",
                           ncol = 2) +
    patchwork::plot_annotation(
      title = "Predicted Probability of Responses to Proposed U.S. Military Facility at...",
      theme = theme(text = element_text(family = "oswald"),
                    plot.title = element_text(family = "oswald",
                                              face = "bold",
                                              size = 16))
      )


  ggsave(here::here("Figures/predicted-prob-combined.png"),
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)

}




# Contrasts based on distance for province level models
figure_province_dist_contrasts_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , str_extract(as.character(modelobject[[1]]$formula[1]), pattern = "\\([^()]+\\)")),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              province = unique(modelobject[[1]]$data$province),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z))

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                               .f = ~ add_epred_draws(modelobject[[.x]],
                                                      newdata = newdataframe,
                                                      ndraws = 500,
                                                      value = ".epred",
                                                      re_formula = re_form
                                                      ) |>
                                 mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                        model = str_extract(model, "\\d+k"))
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, .category, .draw)  |>
    group_by(province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category != "DKDA") |>
    mutate(.category = factor(.category,
                              levels = c("Neutral", "Support", "Oppose")))

  # Write a list of the various group pairings/contrasts
  plotgroups <- as.list(unique(plotlist$group_term))

  # Use the data frames from plotlist to create ggplot objects for each possible
  # value of the contrasts in plotgroups.
  plotout <- furrr::future_map(.x = seq_along(plotgroups),
                              .f = ~ ggplot(data = plotlist |>
                                              filter(group_term == plotgroups[[.x]]),
                                            aes(x = .epred,
                                                fill = after_stat(x > 0),
                                                group = .category)) +
                                stat_dotsinterval(point_interval = "median_qi",
                                                  scale = 0.9,
                                                  overflow = "compress",
                                                  quantiles = 100,
                                                  .width = c(0.50, 0.89),
                                                  slab_linewidth = 0,
                                                  point_size = 3,
                                                  color = "black",
                                                  show.legend = FALSE
                                                  ) +
                                geom_vline(xintercept = 0,
                                           linewidth = 1.1,
                                           linetype = "dashed",
                                           color = "black") +
                                facet_wrap(. ~ .category,
                                           ncol = 1) +
                                geom_label(
                                  aes(label = pd, y = 0.98, x = median),
                                  fill = "white",
                                  data = \(d) d |>
                                    group_by(.category) |>
                                    dplyr::summarise(pd = paste0("Pr(Direction) =", round(bayestestR::pd(.epred), 3)),
                                                     median = median(.epred))
                                ) +
                                scale_y_continuous(expand = c(0.1,0.1)) +
                                viridis::scale_fill_viridis(discrete = TRUE,
                                                            option = "turbo",
                                                            direction = -1,
                                                            begin = 0.1,
                                                            end = 1.0) +
                                theme_flynn(base_family = "oswald") +
                                theme(axis.text.y.left = element_blank(),
                                      axis.ticks.y.left = element_blank()) +
                                guides(color = guide_legend(reverse = TRUE),
                                       fill = guide_legend(reverse = TRUE)) +
                                labs(x = "Change in Predicted Probability",
                                     y = "",
                                     fill = "Outcome Category",
                                     color = "Outcome Category",
                                     title = glue::glue("Contrasts between {plotgroups[[.x]]}"),
                                     subtitle = "Varying intercepts on province"
                                     )
                              )
  # Write a list of the file names to be used when saving ggplot objects.
  plotnames <- map(.x = plotgroups,
                   .f = ~ str_to_lower(gsub(" ", "", glue::glue("contrasts-{.x}.png"))))

  # Save ggplot objects
  pwalk(list(plotnames, plotout),
        ggsave,
        height = 6,
        width = 7,
        units = "in",
        path = here::here("Figures/"))

}




# Contrasts based on distance for province level models

figure_province_contrasts_map_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , str_extract(as.character(modelobject[[1]]$formula[1]), pattern = "\\([^()]+\\)")),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              province = unique(modelobject[[1]]$data$province),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z))

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                               .f = ~ add_epred_draws(modelobject[[.x]],
                                                      newdata = newdataframe,
                                                      ndraws = 500,
                                                      value = ".epred",
                                                      re_formula = re_form
                                                      ) |>
                                 mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                        model = str_extract(model, "\\d+k"))
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, .category, .draw)  |>
    group_by(province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category %in% c("Support", "Oppose")) |>
    filter(group_term %in% c("Control: 5k - Control: 100k",
                             "Economic: 5k - Economic: 100k",
                             "Security: 5k - Security: 100k",
                             "Security and Economic: 5k - Security and Economic: 100k")
           ) |>
    mutate(group_term = case_when(
      grepl(".*Control.*", group_term) ~ "Control",
      grepl("^Economic: 5k", group_term) ~ "Economic",
      grepl("^Security: 5k", group_term) ~ "Security",
      grepl(".*Security and.*", group_term) ~ "Security and Economic"
      )
      ) |>
    group_by(province, .category, group_term) |>
    dplyr::summarise(median = median(.epred))

  map <- geodata::gadm("POL", level = 1, path = here::here("Data")) |>
    sf::st_as_sf() |>
    cross_join(plotlist) |>
    filter(NAME_1 == province)

  ggplot(data = map) +
    geom_sf(aes(geometry = geometry,
                fill = median
                ),
            size = 0.1) +
    facet_grid(.category ~ group_term,
               switch = "y") +
    theme_void() +
    theme(text = element_text(family = "oswald"),
          plot.title.position = "plot",
          plot.title = element_text(size = 20,
                                    face = "bold",
                                    family = "oswald",
                                    margin = margin(b = 0.2, unit = "cm")),
          plot.subtitle = element_text(size = 16,
                                       family = "oswald",
                                       margin = margin(b = 0.2, unit = "cm")),
          strip.text = element_text(size = 12),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          legend.position = "bottom",
          legend.key.width = unit(2, units = "cm"),
          ) +
    viridis::scale_fill_viridis(option = "turbo",
                                direction = -1) +
    labs(title = "Median posterior contrasts by province and response",
         fill = "Contrast from distance")

  ggsave(here::here("Figures/map-province.png"),
         height = 5,
         width = 7,
         units = "in")

}



# Contrasts based on distance for province level models

figure_district_contrasts_map_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , "(1 | province) + (1 | province:district)"),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Generate synthetic data onto which we can generate predictions/expected values
  location.list <- unique(glue::glue("{modelobject[[1]]$data$province}:{modelobject[[1]]$data$district}"))

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(district = unique(modelobject[[1]]$data$district),
                              province = unique(modelobject[[1]]$data$province),
                              treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z)
  ) |>
    filter(glue::glue("{province}:{district}") %in% location.list)

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                               .f = ~ add_epred_draws(modelobject[[.x]],
                                                      newdata = newdataframe,
                                                      ndraws = 500,
                                                      value = ".epred",
                                                      re_formula = re_form
                                                      ) |>
                                 mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                        model = str_extract(model, "\\d+k")
                                        )
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, district, .category, .draw)  |>
    dplyr::group_by(province, district, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category %in% c("Support", "Oppose")) |>
    filter(group_term %in% c("Control: 5k - Control: 100k",
                             "Economic: 5k - Economic: 100k",
                             "Security: 5k - Security: 100k",
                             "Security and Economic: 5k - Security and Economic: 100k")) |>
    mutate(group_term = case_when(
      grepl(".*Control.*", group_term) ~ "Control",
      grepl("^Economic: 5k", group_term) ~ "Economic",
      grepl("^Security: 5k", group_term) ~ "Security",
      grepl(".*Security and.*", group_term) ~ "Security and Economic"
    )) |>
    dplyr::group_by(province, district, .category, group_term) |>
    dplyr::summarise(median = median(.epred))

  map <- geodata::gadm("POL", level = 2, path = here::here("Data")) |>
    sf::st_as_sf() |>
    cross_join(plotlist) |>
    filter(NAME_1 == province & NAME_2 == district)

  ggplot(data = map) +
    geom_sf(aes(geometry = geometry, fill = median), size = 0.1) +
    facet_grid(.category ~ group_term,
               switch = "y") +
    theme_void() +
    theme(text = element_text(family = "oswald"),
          plot.title.position = "plot",
          plot.title = element_text(size = 20,
                                    face = "bold",
                                    family = "oswald",
                                    margin = margin(b = 0.2, unit = "cm")),
          plot.subtitle = element_text(size = 16,
                                       family = "oswald",
                                       margin = margin(b = 0.2, unit = "cm")),
          strip.text = element_text(size = 12),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          legend.position = "bottom",
          legend.key.width = unit(2, units = "cm")) +
    viridis::scale_fill_viridis(option = "turbo",
                                direction = -1) +
    labs(title = "Median posterior contrasts by district and response",
         fill = "Contrast from distance")

  ggsave(here::here("Figures/map-district.png"),
         height = 5,
         width = 7,
         units = "in")



}





# Contrasts based on distance for province level models

figure_district_dist_contrasts_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , "(1 | province) + (1 | province:district)"),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  location.list <- unique(glue::glue("{modelobject[[1]]$data$province}:{modelobject[[1]]$data$district}"))

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(district = unique(modelobject[[1]]$data$district),
                             province = unique(modelobject[[1]]$data$province),
                             treatment_group = unique(modelobject[[1]]$data$treatment_group),
                             minority = names(which.max(table(modelobject[[1]]$data$minority))),
                             gender = names(which.max(table(modelobject[[1]]$data$gender))),
                             age = names(which.max(table(modelobject[[1]]$data$age))),
                             income = names(which.max(table(modelobject[[1]]$data$income))),
                             income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                             education = names(which.max(table(modelobject[[1]]$data$education))),
                             ideology_z = mean(modelobject[[1]]$data$ideology_z)
                             ) |>
    filter(glue::glue("{province}:{district}") %in% location.list)


  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                                .f = ~ add_epred_draws(modelobject[[.x]],
                                                       newdata = newdataframe,
                                                       ndraws = 500,
                                                       value = ".epred",
                                                       re_formula = re_form
                                                       ) |>
                                  mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                         model = str_extract(model, "\\d+k"))
                                ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, district, .category, .draw)  |>
    group_by(province, district, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category != "DKDA") |>
    mutate(.category = factor(.category,
                              levels = c("Neutral", "Support", "Oppose")))

  # Write a list of the various group pairings/contrasts
  plotgroups <- as.list(unique(plotlist$group_term))

  # Use the data frames from plotlist to create ggplot objects for each possible
  # value of the contrasts in plotgroups.
  plotout <- furrr::future_map(.x = seq_along(plotgroups),
                               .f = ~ ggplot(data = plotlist |> filter(group_term == plotgroups[[.x]]),
                                             aes(x = .epred,
                                                 fill = after_stat(x > 0)
                                                 )
                                             ) +
                                 stat_dotsinterval(scale = 0.9,
                                                   overflow = "compress",
                                                   quantiles = 100,
                                                   .width = c(0.50, 0.89),
                                                   slab_linewidth = 0,
                                                   point_size = 3,
                                                   color = "black",
                                                   show.legend = FALSE
                                                   ) +
                                 geom_vline(xintercept = 0,
                                            linewidth = 1.1,
                                            linetype = "dashed",
                                            color = "black") +
                                 facet_wrap(. ~ .category,
                                            ncol = 1) +
                                 geom_label(
                                   aes(label = pd, y = 0.98, x = median),
                                   fill = "white",
                                   data = \(d) d |>
                                     group_by(.category) |>
                                     dplyr::summarise(pd = paste0("Pr(Direction) =", round(bayestestR::pd(.epred), 3)),
                                                      median = median(.epred))
                                 ) +
                                 scale_y_continuous(expand = c(0.1,0.1)) +
                                 viridis::scale_fill_viridis(discrete = TRUE,
                                                             option = "turbo",
                                                             direction = -1,
                                                             begin = 0.1,
                                                             end = 1.0) +
                                 theme_flynn(base_family = "oswald") +
                                 theme(axis.text.y.left = element_blank(),
                                       axis.ticks.y.left = element_blank()) +
                                 guides(color = guide_legend(reverse = TRUE),
                                        fill = guide_legend(reverse = TRUE)) +
                                 labs(x = "Change in Predicted Probability",
                                      y = "",
                                      fill = "Outcome Category",
                                      color = "Outcome Category",
                                      title = glue::glue("Contrasts between {plotgroups[[.x]]}"),
                                      subtitle = "Varying intercepts on province and district"
                   )
  )
  # Write a list of the file names to be used when saving ggplot objects.
  plotnames <- map(.x = plotgroups,
                   .f = ~ str_to_lower(gsub(" ", "", glue::glue("contrasts-district-{.x}.png"))))

  # Save ggplot objects
  pwalk(list(plotnames, plotout),
        ggsave,
        height = 6,
        width = 7,
        units = "in",
        path = here::here("Figures/"))

}









# Contrasts based on distance for province level models with contact interaction
figure_province_dist_contact_contrasts_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , str_extract(as.character(modelobject[[1]]$formula[1]), pattern = "\\([^()]+\\)")),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              contact_pers = unique(modelobject[[1]]$data$contact_pers),
                              province = unique(modelobject[[1]]$data$province),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z))

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                               .f = ~ add_epred_draws(modelobject[[.x]],
                                                      newdata = newdataframe,
                                                      ndraws = 500,
                                                      value = ".epred",
                                                      re_formula = re_form
                                                      ) |>
                                 mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                        model = str_extract(model, "\\d+k"))
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, contact_pers, province, .category, .draw)  |>
    group_by(contact_pers, province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    compare_levels(variable = .epred,
                   by = contact_pers) |>
    filter(.category != "DKDA") |>
    filter(contact_pers == "Yes - No") |>
    mutate(.category = factor(.category,
                              levels = c("Neutral", "Support", "Oppose")))

  # Write a list of the various group pairings/contrasts
  plotgroups <- as.list(unique(plotlist$group_term))

  # Use the data frames from plotlist to create ggplot objects for each possible
  # value of the contrasts in plotgroups.
  plotout <- furrr::future_map(.x = seq_along(plotgroups),
                               .f = ~ ggplot(data = plotlist |> filter(group_term == plotgroups[[.x]]),
                                             aes(x = .epred,
                                                 fill = after_stat(x > 0)
                                                 )
                                             ) +
                                 stat_dotsinterval(scale = 0.9,
                                                   #overflow = "compress",
                                                   quantiles = 100,
                                                   .width = c(0.50, 0.89),
                                                   slab_linewidth = 0,
                                                   point_size = 3,
                                                   color = "black",
                                                   show.legend = FALSE
                                                   ) +
                                 geom_vline(xintercept = 0,
                                            linewidth = 1.1,
                                            linetype = "dashed",
                                            color = "black") +
                                 facet_wrap(. ~ .category,
                                            ncol = 1) +
                                 geom_label(
                                   aes(label = pd, y = 0.98, x = median),
                                   fill = "white",
                                   data = \(d) d |>
                                     group_by(.category) |>
                                     dplyr::summarise(pd = paste0("Pr(Direction) =", round(bayestestR::pd(.epred), 3)),
                                                      median = median(.epred))
                                 ) +
                                 scale_y_continuous(expand = c(0.1,0.1)) +
                                 viridis::scale_fill_viridis(discrete = TRUE,
                                                             option = "turbo",
                                                             direction = -1,
                                                             begin = 0.1,
                                                             end = 1.0) +
                                 theme_flynn(base_family = "oswald") +
                                 theme(axis.text.y.left = element_blank(),
                                       axis.ticks.y.left = element_blank()) +
                                 guides(color = guide_legend(reverse = TRUE),
                                        fill = guide_legend(reverse = TRUE)) +
                                 labs(x = "Change in Predicted Probability",
                                      y = "",
                                      fill = "Outcome Category",
                                      color = "Outcome Category",
                                      title = glue::glue("Contrasts between Contact = Yes and Contact = No for {plotgroups[[.x]]}"),
                                      subtitle = "Varying intercepts on province"
                                      )
                               )
  # Write a list of the file names to be used when saving ggplot objects.
  plotnames <- map(.x = plotgroups,
                   .f = ~ str_to_lower(gsub(" ", "", glue::glue("contact-contrasts-{.x}.png"))))

  # Save ggplot objects
  pwalk(list(plotnames, plotout),
        ggsave,
        height = 6,
        width = 8,
        units = "in",
        path = here::here("Figures/"))

}




# Contrasts based on distance for province level models with contact interaction
figure_district_dist_contact_contrasts_f <- function(modelobject, group.effects) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , "(1 | province) + (1 | province:district)"),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Create location list that identifies unique pairs of provinces and districts appearing in data.
  location.list <- unique(glue::glue("{modelobject[[1]]$data$province}:{modelobject[[1]]$data$district}"))

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(district = unique(modelobject[[1]]$data$district),
                              province = unique(modelobject[[1]]$data$province),
                              contact_pers = unique(modelobject[[1]]$data$contact_pers),
                              treatment_group = unique(modelobject[[1]]$data$treatment_group),
                              minority = names(which.max(table(modelobject[[1]]$data$minority))),
                              gender = names(which.max(table(modelobject[[1]]$data$gender))),
                              age = names(which.max(table(modelobject[[1]]$data$age))),
                              income = names(which.max(table(modelobject[[1]]$data$income))),
                              income_source = names(which.max(table(modelobject[[1]]$data$income_source))),
                              education = names(which.max(table(modelobject[[1]]$data$education))),
                              ideology_z = mean(modelobject[[1]]$data$ideology_z)
  ) |>
    filter(glue::glue("{province}:{district}") %in% location.list)

  # Begin list of plots.
  # First start with model objects and then create data frames containing expected
  # values using the newdataframe above.
  #
  # add_epred_draws() will produce predicted probability values {0,1}

  # add_predicted_draws() produces random draws from the posterior distribution
  # that take on the factor levels of the outcome variable from the original
  # model (i.e. Support, Oppose, etc.)
  plotlist <- furrr::future_map(.x = seq_along(modelobject),
                                .f = ~ add_epred_draws(modelobject[[.x]],
                                                       newdata = newdataframe,
                                                       ndraws = 500,
                                                       value = ".epred",
                                                       re_formula = re_form
                                ) |>
                                  mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                         model = str_extract(model, "\\d+k"))
  ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, contact_pers, province, district, .category, .draw)  |>
    group_by(contact_pers, province, district, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    compare_levels(variable = .epred,
                   by = contact_pers) |>
    filter(.category != "DKDA") |>
    filter(contact_pers == "Yes - No") |>
    mutate(.category = factor(.category,
                              levels = c("Neutral", "Support", "Oppose")))

  # Write a list of the various group pairings/contrasts
  plotgroups <- as.list(unique(plotlist$group_term))

  # Use the data frames from plotlist to create ggplot objects for each possible
  # value of the contrasts in plotgroups.
  plotout <- furrr::future_map(.x = seq_along(plotgroups),
                               .f = ~ ggplot(data = plotlist |> filter(group_term == plotgroups[[.x]]),
                                             aes(x = .epred,
                                                 fill = after_stat(x > 0)
                                                 )
                                             ) +
                                 stat_dotsinterval(scale = 0.9,
                                                   #overflow = "compress",
                                                   quantiles = 100,
                                                   .width = c(0.50, 0.89),
                                                   slab_linewidth = 0,
                                                   point_size = 3,
                                                   color = "black",
                                                   show.legend = FALSE
                                 ) +
                                 geom_vline(xintercept = 0,
                                            linewidth = 1.1,
                                            linetype = "dashed",
                                            color = "black") +
                                 facet_wrap(. ~ .category,
                                            ncol = 1) +
                                 geom_label(
                                   aes(label = pd, y = 0.98, x = median),
                                   fill = "white",
                                   data = \(d) d |>
                                     group_by(.category) |>
                                     dplyr::summarise(pd = paste0("Pr(Direction) =", round(bayestestR::pd(.epred), 3)),
                                                      median = median(.epred))
                                 ) +
                                 scale_y_continuous(expand = c(0.1,0.1)) +
                                 viridis::scale_fill_viridis(discrete = TRUE,
                                                             option = "turbo",
                                                             direction = -1,
                                                             begin = 0.1,
                                                             end = 1.0) +
                                 theme_flynn(base_family = "oswald") +
                                 theme(axis.text.y.left = element_blank(),
                                       axis.ticks.y.left = element_blank()) +
                                 guides(color = guide_legend(reverse = TRUE),
                                        fill = guide_legend(reverse = TRUE)) +
                                 labs(x = "Change in Predicted Probability",
                                      y = "",
                                      fill = "Outcome Category",
                                      color = "Outcome Category",
                                      title = glue::glue("Contrasts between Contact = Yes and Contact = No for {plotgroups[[.x]]}"),
                                      subtitle = "Varying intercepts on province and district"
                                 )
  )
  # Write a list of the file names to be used when saving ggplot objects.
  plotnames <- map(.x = plotgroups,
                   .f = ~ str_to_lower(gsub(" ", "", glue::glue("contact-district-contrasts-{.x}.png"))))

  # Save ggplot objects
  pwalk(list(plotnames, plotout),
        ggsave,
        height = 6,
        width = 8,
        units = "in",
        path = here::here("Figures/"))


}
# Test
