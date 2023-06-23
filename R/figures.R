

sysfonts::font_add_google("Oswald", family = "oswald")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Types of contact
figure_contact_type_f <- function(data) {

  barcolor <- viridis::turbo(1, begin = 0.10)

  p1 <- ggplot(data = data,
         aes(y = contact_pers,
             x = after_stat(count/sum(count)))) +
    geom_bar(fill = barcolor) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, 1),
                       expand = c(0, 0.01)) +
    scale_y_discrete(labels = c("No",
                                "Yes",
                                "Don't know/Decline to answer")) +
    labs(x = "Percent",
         y = "Reported Contact") +
    theme_flynn()

    tempdata <- data |>
      dplyr::select(contact_pers_type) |>
      tidyr::separate_longer_delim(contact_pers_type,
                                   delim = stringr::regex("\\.\\,|\\,(?=[A-Z])")) |>  # This regex identifies a comma followed by a period, or a comma immediately followed by a capital letter, but uses the lookahead question mark thing to ignore the capital letter so it doesn't include it as a delimiter.
      dplyr::group_by(contact_pers_type) |>
      dplyr::summarise(total = n()) |>
      filter(!is.na(contact_pers_type))

  p2 <- ggplot(data = tempdata,
           aes(y = reorder(contact_pers_type, total),
               x = total)) +
      geom_bar(fill = barcolor,
               stat = "identity") +
      scale_x_continuous(expand = c(0, 1)) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 55)) +
      labs(x = "Count",
           y = "Contact Type") +
    theme_flynn() +
    theme(axis.text.y.left = element_text(size = 7.5,
                                          hjust = 0,
                                          vjust = 0,
                                          margin = margin(r = -5.0, b = 0.5, unit = "cm")),
          axis.ticks.length.y.left = unit(5.5, "cm"),
          axis.ticks.y.left = element_line(linetype = "dotted",
                                           linewidth = 0.3))

  patchwork::wrap_plots(p1, p2) +
    patchwork::plot_layout(ncol = 2) &
    theme(text = element_text(family = "Oswald"))

  ggsave(here("Figures/contact-type-us-troops.png"), dpi = 300, width = 10, height = 7)

}




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

  ggsave(here("Figures/views-us-troops.png"), dpi = 300, width = 8, height = 5)

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

  ggsave(here("Figures/views-russian-relations.png"), dpi = 300, width = 8, height = 5)

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


# Distribution of observations across provinces and districts

figure_observation_distribution_f <- function(data) {

  # Set color for bar fill
  barcolor <- viridis::turbo(1, begin = 0.10)

temp.prov <- data |>
  dplyr::group_by(province) |>
  dplyr::summarise(obs = n())

ggplot(data = temp.prov,
       aes(x = obs)) +
  geom_histogram(bins = 50,
                 fill = barcolor) +
  scale_x_continuous(limits = c(0, 350)) +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  theme_flynn() +
  labs(x = "Respondents Per Province",
       y = "Number of Provinces",
       title = "Distribution of respondent count per province")

ggsave(here::here("Figures/distribution-responses-province.png"),
       dpi = 300,
       height = 5,
       width = 8,
       units = "in")

temp.dist <- data |>
  dplyr::group_by(province, district) |>
  dplyr::summarise(obs = n())

ggplot(data = temp.dist,
       aes(x = obs)) +
  geom_histogram(bins = 50,
                 fill = barcolor) +
  scale_x_continuous(limits = c(0, 160)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  theme_flynn() +
  labs(x = "Respondents Per District",
       y = "Number of Districts",
       title = "Distribution of respondent count per district")

ggsave(here::here("Figures/distribution-responses-district.png"),
       dpi = 300,
       height = 5,
       width = 8,
       units = "in")

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
                 model = str_extract(model, "\\d+km"))
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
                                                        begin = 0.01,
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
                                              size = 15))
      )


  ggsave(here::here("Figures/predicted-prob-combined.png"),
         width = 8,
         height = 6,
         units = "in",
         dpi = 300)

  return(plotlist)
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
                              ideology_z = mean(modelobject[[1]]$data$ideology_z)
                              )

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
                                        model = str_extract(model, "\\d+km"))
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
                                      axis.ticks.y.left = element_blank(),
                                      plot.margin = margin(t = 0.10,
                                                           b = 0.10,
                                                           l = 0.10,
                                                           r = 0.21,
                                                           unit = "cm"
                                                           )
                                      ) +
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

  # Generate combined plot for select panels to show substantive treatment effects.
  # Only includes contrasts between treatment levels within distance question.

  templist <- list(plotout[[2]], plotout[[9]],
                   plotout[[6]], plotout[[13]],
                   plotout[[4]], plotout[[11]])

  plots <- furrr::future_map(
    .x = templist,
    .f = ~ {
      plot <- .x
      plot$labels$title <- stringr::str_replace(as.character(plot$labels$title), "Contrasts between ", "")
      plot$labels$subtitle <- stringr::str_replace(as.character(plot$labels$subtitle), "Varying intercepts on province", "")

      plot

  })

  patchwork::wrap_plots(plots) +
    patchwork::plot_layout(guides = "collect",
                           ncol = 2) +
    patchwork::plot_annotation(
      title = "Contrasts between treatment categories",
      #subtitle = "Varying intercepts on province",
      theme = theme(text = element_text(family = "oswald"),
                    plot.margin = margin(),
                    plot.title = element_text(family = "oswald",
                                              face = "bold",
                                              size = 20,
                                              margin = margin(b = 0.1, unit = "cm")))
    ) &
    theme(plot.title = element_text(margin = margin(b = -0.0, unit = "cm")))


  ggsave(here::here("Figures/contrasts-treatment-province-combined.png"),
         width = 9,
         height = 14,
         units = "in",
         dpi = 300)



  # Figures for within-treatment-category comparison based on distance.
  templist <- list(plotout[[1]], plotout[[14]], plotout[[23]], plotout[[28]]
                   )

plots <- furrr::future_map(
  .x = templist,
  .f = ~ {
    plot <- .x
    plot$labels$title <- stringr::str_replace(as.character(plot$labels$title), "Contrasts between ", "")
    plot$labels$subtitle <- stringr::str_replace(as.character(plot$labels$subtitle), "Varying intercepts on province", "")

    plot
  })

patchwork::wrap_plots(plots) +
  patchwork::plot_layout(guides = "collect",
                         ncol = 2) +
  patchwork::plot_annotation(
    title = "Within-treatment contrasts by distance",
    #subtitle = "Varying intercepts on province",
    theme = theme(text = element_text(family = "oswald"),
                  plot.margin = margin(),
                  plot.title = element_text(family = "oswald",
                                            face = "bold",
                                            size = 20,
                                            margin = margin(b = 0.2, unit = "cm")))
  ) &
  theme(plot.title = element_text(margin = margin(b = 0.1, unit = "cm")))


ggsave(here::here("Figures/contrasts-treatment-comparison-combined.png"),
       width = 12,
       height = 11,
       units = "in",
       dpi = 300)

  return(plotout)

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
                                        model = str_extract(model, "\\d+km"))
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, .category, .draw)  |>
    group_by(province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category %in% c("Support", "Oppose")) |>
    filter(group_term %in% c("Control: 5km - Control: 100km",
                             "Economic: 5km - Economic: 100km",
                             "Security: 5km - Security: 100km",
                             "Security and Economic: 5km - Security and Economic: 100km")
           ) |>
    mutate(group_term = case_when(
      grepl(".*Control.*", group_term) ~ "Control",
      grepl("^Economic: 5km", group_term) ~ "Economic",
      grepl("^Security: 5km", group_term) ~ "Security",
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

    re_form <- print(paste( "~" , "(1 |ID1| province) + (1 |ID2| province:district)"),
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
                                        model = str_extract(model, "\\d+km")
                                        )
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, district, .category, .draw)  |>
    dplyr::group_by(province, district, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
    filter(.category %in% c("Support", "Oppose")) |>
    filter(group_term %in% c("Control: 5km - Control: 100km",
                             "Economic: 5km - Economic: 100km",
                             "Security: 5km - Security: 100km",
                             "Security and Economic: 5km - Security and Economic: 100km")) |>
    mutate(group_term = case_when(
      grepl(".*Control.*", group_term) ~ "Control",
      grepl("^Economic: 5km", group_term) ~ "Economic",
      grepl("^Security: 5km", group_term) ~ "Security",
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

    re_form <- print(paste( "~" , "(1 |ID1| province) + (1 |ID2| province:district)"),
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
                                         model = str_extract(model, "\\d+km"))
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
                                                             begin = 0.101,
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

  return(plotout)
}



# Look at the effect of contrast on treatment
# # Contrasts based on distance for province level models with contact interaction
figure_province_dist_contact_treatment_effect_f <- function(modelobject, group.effects) {

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
                              ideology_z = mean(modelobject[[1]]$data$ideology_z)) |>
    filter(!grepl(".*Don.*", contact_pers))

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
                                         model = str_extract(model, "\\d+km"))
  ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: Contact {contact_pers} at {model}"))) |>
    dplyr::select(.epred, group_term, province, .category, .draw)  |>
    filter(.category != "DKDA") |>
    group_by(province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
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
                                      title = glue::glue("Contrasts between {plotgroups[[.x]]}"),
                                      subtitle = "Varying intercepts on province"
                                 )
  )
  # Write a list of the file names to be used when saving ggplot objects.
  plotnames <- map(.x = plotgroups,
                   .f = ~ str_to_lower(gsub(" ", "", glue::glue("contact-conditioning-contrasts-treatment-{.x}.png"))))

  # Save ggplot objects
  pwalk(list(plotnames, plotout),
        ggsave,
        height = 6,
        width = 8,
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
                              ideology_z = mean(modelobject[[1]]$data$ideology_z)) |>
    filter(!grepl(".*Don.*", contact_pers))

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
                                        model = str_extract(model, "\\d+km"))
                               ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: Contact {contact_pers} at {model}"))) |>
    dplyr::select(.epred, group_term, province, .category, .draw)  |>
    filter(.category != "DKDA") |>
    group_by(province, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
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
                                      title = glue::glue("Contrasts for {plotgroups[[.x]]}"),
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

    re_form <- print(paste( "~" , "(1 |ID1| province) + (1 |ID2| province:district)"),
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
                                         model = str_extract(model, "\\d+km"))
  ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: Contact {contact_pers} at {model}"))) |>
    dplyr::select(.epred, group_term, province, district, .category, .draw)  |>
    filter(.category != "DKDA") |>
    group_by(province, district, .category) |>
    compare_levels(variable = .epred,
                   by = group_term) |>
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
                                      title = glue::glue("Contrasts for {plotgroups[[.x]]}"),
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
