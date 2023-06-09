
library(tidyverse)
library(geodata)

provinces <- geodata::gadm("POL", level = 2, path = here::here("Data"))

provlist <- data.frame("province" = provinces$NAME_1,
                       "district" = provinces$NAME_2)

readr::write_csv(provlist,
                 here::here("Data/district-list.csv"),
                 quote = "all",
                 col_names = FALSE)
