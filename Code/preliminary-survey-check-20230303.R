library(tidyverse)
library(data.table)
library(here)

data <- fread(here("Data/Poland Survey 2023_March 3, 2023_08.23.csv")) |>
  slice(-c(1:2))


# Check gender
# Only 35% women
table(data$Q2)

length(data$Q2[data$Q2=="Female"])/length(data$Q2)


# Minority
table(data$Q3)
length(data$Q3[data$Q3=="Yes"])/length(data$Q3)


# Education
table(data$Q4)


# Age
# Older age cohorts a bit low
table(data$Q5)


# Province
table(data$Q6_1)

# District
table(data$Q6_2)


# Income.
# Lots of respondents in highest income grouping.
table(data$Q9)


# Primary income
table(data$Q59)


# Religion
table(data$Q10)


# Ideology
table(as.numeric(data$Q11_1))


# Treatment groups
# The number returned indicates how many observations received each treatment.
# Approximately 75% should be missing with 25% receiving the treatment for
# each individual treatment category.
# First group
length(data$Q61[data$Q61!=""])/length(data$Q61)
length(data$Q63[data$Q63!=""])/length(data$Q63)

# Second group
length(data$Q64[data$Q64!=""])/length(data$Q64)
length(data$Q65[data$Q65!=""])/length(data$Q65)

# Third group
length(data$Q68[data$Q68!=""])/length(data$Q68)
length(data$Q69[data$Q69!=""])/length(data$Q69)

# Fourth group
length(data$Q66[data$Q66!=""])/length(data$Q66)
length(data$Q67[data$Q67!=""])/length(data$Q67)


#### Update income quintiles using information from Polish Statistics Office ####

quintiles <- data.frame(avgmonthlyincome = c(1214.29, 1675.00, 2137.50, 2800.00, NA),
                        householdsize = rep(2.5, 5),
                        months = rep(12, 5)) |>
  mutate(averageannualincome = avgmonthlyincome * householdsize * months,
         difference = averageannualincome - lag(averageannualincome),
         quintiles = averageannualincome + lead(difference/2),
         quintiles = ifelse(averageannualincome == 84000, averageannualincome + difference/2, quintiles),
         quintiles = ifelse(is.na(quintiles), glue::glue("> {lag(quintiles)}"), quintiles))

