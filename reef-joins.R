library(tidyverse)

moorea_coral <- read_csv(
  "data/moorea_coral.csv",
  na = c("", "NA", "ND") # This vector tells read_csv() which values to interpret as missing data
)

moorea_fish <- read_csv(
  "data/moorea_fish.csv",
  na = c("", "NA", "ND")
)

glimpse(moorea_coral)
glimpse(moorea_fish)

non_coral <- c(
  "Sand",
  "CTB",
  "Macroalgae",
  "Non-coralline Crustose Algae",
  "Unknown or Other"
)

coral_depth <- moorea_coral |>
  filter(!(Taxonomy_Substrate_or_Functional_Group %in% non_coral), Depth < 17)

coral_depth_Y <- coral_depth |>
  mutate(
    Year = as.integer(str_sub(Date, start = 1L, end = 4L))
  )

#Use mutate(), str_sub(), and as.numeric() to pull the four-digit
# year out of Date (which is formatted "YYYY-MM")
# into a new column called Year.Year=str_sub(Year, (start = 1L, stop = 4L))
