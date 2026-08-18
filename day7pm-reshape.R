library(tidyverse)


surveys <- read_csv("data/surveys.csv")
species <- read_csv("data/species.csv")
plots <- read_csv("data/plots.csv")


# Pivot_Wider() ----------------------------------------------------------

# Summarize our surveys to get the mean wight by sex

weight_by_sex <- surveys |>
  filter(!is.na(sex)) |>
  summarize(mean_weight = mean(weight, na.rm = TRUE), .by = c(species_id, sex))

glimpse(weight_by_sex)


# Pivot wider to see the mean weight by sex in columns

weight_by_sex_wider <- weight_by_sex |>
  pivot_wider(
    # where do we get the _names_ of the new columnss from?
    names_from = sex,
    values_from = mean_weight
  )
weight_by_sex_wider


# Pivot wider is usefiul when one columcn contans multiple sets
#  of values that you want to use with each other

#another example
taxa_by_year <- surveys |>
  # choses a subset for years for convenience
  filter(year >= 1999) |>
  #this join will add the taxa column from species to my surveys subset
  inner_join(
    select(species, species_id, taxa), #species_id is already joined in common, will only add 1 column
    join_by(species_id)
  ) |>
  count(year, taxa)
taxa_by_year


#pivot this wider so we have columns for birds and rodents

taxa_wider <- taxa_by_year |>
  pivot_wider(
    names_from = taxa,
    values_from = n
  )
taxa_wider


# Pivot longer() ---------------------------------------------------------

# Pivot back to long format

taxa_wider |>
  pivot_longer(
    #what columnsare we pivoting
    cols = !year,
    #where are the names going
    names_to = "taxa",
    #where are the values going
    values_to = "n"
  )
