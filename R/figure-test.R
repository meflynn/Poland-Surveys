# Contrasts based on distance for province level models
figure_contrast_test_f <- function(modelobject, group.effects, response.cat) {

  # Decide whether or not to include varying intercept term when generating predictions
  # This just pulls the varying effects/intercepts term from the model formula.
  if (group.effects == TRUE) {

    re_form <- print(paste( "~" , str_extract(as.character(modelobject[[1]]$formula[1]), pattern = "\\([^()]+\\)")),
                     quote = FALSE)

  } else {

    re_form <- NA

  }

  # Generate synthetic data onto which we can generate predictions/expected values
  newdataframe <- expand_grid(treatment_group = c("Control", "Security"),
                              province = unique(modelobject[[1]]$data$province),
                              minority = unique(modelobject[[1]]$data$minority),
                              gender = unique(modelobject[[1]]$data$gender),
                              age = unique(modelobject[[1]]$data$age),
                              income = unique(modelobject[[1]]$data$income),
                              income_source = unique(modelobject[[1]]$data$income_source),
                              education = unique(modelobject[[1]]$data$education),
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
                                                       ndraws = 100,
                                                       value = ".epred",
                                                       re_formula = re_form,
                                                       .category = response.cat
                                ) |>
                                  mutate(model = glue::glue("{modelobject[[.x]]$formula[[5]]}"),
                                         model = str_extract(model, "\\d+km"),
                                         model = gsub("(\\d)(km)", "\\1 \\2", model))
  ) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(group_term = factor(glue::glue("{treatment_group}: {model}"))) |>
    dplyr::select(.epred, group_term, province, minority, gender, age, income, income_source, education, .category, .draw)  |>
    group_by(province, .category, minority, gender, age, income, income_source, education) |>
    filter(.category != "DKDA") |>
    compare_levels(variable = .epred,
                   by = group_term,
                   ignore_groups = ".row") |>
    mutate(.category = factor(.category,
                              levels = c(response.cat)))

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

  return(plotout)

}
