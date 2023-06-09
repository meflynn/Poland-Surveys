# Tables for the Poland manuscript




# Balance table for covariate values across treatment groups

table_balance_f <- function(data) {

  varlist <- list("gender", "minority", "education", "age", "income", "income_source", "ideology_z")

  tempdata <- lapply(varlist, function(x) {

    if(x != "ideology_z") {

    tempout <- data |>
      tidyr::drop_na(treatment_group, gender, minority, education, age, income, income_source, ideology_z) |>
      dplyr::select(treatment_group, !!!rlang::syms(x)) |>
      group_by(treatment_group, !!!rlang::syms(x)) |>
      dplyr::summarise(count = n())

    }

    else {

      tempout2 <- data |>
        tidyr::drop_na(treatment_group, gender, minority, education, age, income, income_source, ideology_z) |>
        dplyr::select(treatment_group, ideology_z) |>
        group_by(treatment_group) |>
        dplyr::summarise(count = mean(ideology_z))

    }
  }
  )

  # Create total mean and SD for ideology variable
  ideology_me <- round(mean(data$ideology_z), 3)
  ideology_sd <- round(sd(data$ideology_z), 3)


  # names_from: The columns to use as column names
  # values_from: column to use as cell values (in this case the count)
  # Also need to use paste0. across() can't evaluate glue's .x variable
  # Note: have to use cur_column() below because referring to .x specifically refers to the cell value, NOT the column name!
  out <- data.table::rbindlist(tempdata, fill = TRUE) |>
    mutate(ideology_z = ifelse(count < 1, "ideology_z", NA)) |>
    pivot_longer(cols = c(2, 4, 5, 6, 7, 8, 9)) |>
    filter(!is.na(value)) |>
    pivot_wider(names_from = treatment_group,
                values_from = count) |>
    mutate(across(c("Control", "Security and Economic", "Economic", "Security"),
                  ~ ifelse(is.na(.x), 0, .x))) |>
    rowwise() |>
    mutate(total = sum(`Control`, `Security and Economic`, `Economic`, `Security`),
           across(c("Control", "Security and Economic", "Economic", "Security"),
                  ~ case_when(
                    name != "ideology_z" ~ paste0(.x, " (", round((.x/total)*100, 1),"%)"),
                    name == "ideology_z" ~ paste0(round(.x, 3)," (",
                                                  round(sd(data$ideology_z[data$treatment_group== cur_column()], na.rm = TRUE), 3),")"))),
           name = case_when(
             name == "income_source" ~ "Income Source",
             name == "ideology_z" ~ "Ideology",
             TRUE ~ name
           ),
           name = str_to_title(name),
           value = case_when(
             value == "ideology_z" ~ "Ideology",
             TRUE ~ value
           ),
           total = ifelse(value == "Ideology", paste0(ideology_me, " (", ideology_sd, ")"), as.character(total))) |>
    dplyr::rename("All Groups" = "total",
                  "Predictor Level" = "value")

  # rle looks at a vector and lists the values in that vector and how many times they appear.
  # Super handy!
  # setNames line converts the list into a value object. This is basically a data frame
  # with named rows and a single column containing the row numbers.
  idx <- rle(out$name)
  idx <- setNames(idx$lengths, idx$values)

  last.line <- length(out$name)

  out <- out |>
    dplyr::select(`Predictor Level`, `Control`, `Security`, `Economic`, `Security and Economic`, `All Groups`) |>
    kableExtra::kbl(format = "html", align = "lrrrrr") |>
    kableExtra::kable_styling(latex_options = c("center", "striped", "scale_down"), font_size = 11, position = "center") |>
    kableExtra::add_header_above(c(" ", "Treatment Group" = 4, " "),
                                   align = c("lcr")) |>
    kableExtra::group_rows(index = idx, bold = TRUE, background = "#3498DB", color = "white") |>
    kableExtra::row_spec(2, hline_after = TRUE) |>
    kableExtra::column_spec(2:5, width = "3.5cm") |>
    kableExtra::row_spec(last.line, hline_after = TRUE)

  return(out)

}
