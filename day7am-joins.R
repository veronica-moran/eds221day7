library(tidyverse)

portal_base_url <- "https://raw.githubusercontent.com/weecology/portal-teachingdb/master/"

# Download data

dir.create("data")

download.file(
  paste0(portal_base_url, "surveys.csv"),
  destfile = "data/surveys.csv"
)

download.file(
  paste0(portal_base_url, "species.csv"),
  destfile = "data/species.csv"
)

download.file(paste0(portal_base_url, "plots.csv"), destfile = "data/plots.csv")

surveys <- read_csv("data/surveys.csv")
species <- read_csv("data/species.csv")
plots <- read_csv("data/plots.csv")

glimpse(surveys)
glimpse(species)
glimpse(plots)
# Keys

# Primary key if it unuquely identifies rows in its table

surveys |>
  count(record_id) |>
  filter(n > 1)

# a foreign key connects to another table, it doesnt have to be unique

surveys |>
  count(species_id) |>
  filter(n > 1)


# Joins
surveys |>
  inner_join(
    species,
    by = join_by(species_id)
  ) |>
  glimpse()


#joins in analysis

survey_plots_90s <- surveys |>
  filter(year >= 1990 & year < 2000) |>
  inner_join(plots, by = join_by(plot_id))

ggplot(
  data = survey_plots_90s,
  mapping = aes(
    x = plot_type
  )
) +
  geom_bar() +
  theme_classic(base_size = 8)

# Did the kangaroo rat exxlosures catch any kangaroo rats?
# Genus = Dipodomys

survey_plots_90s |>
  #joins 90s surveys to species
  inner_join(species, by = join_by(species_id)) |>
  #filter to KRAT exclosure
  filter(
    plot_type == "Short-term Krat Exclosure" |
      plot_type == "Long-term Krat Exclosure"
  ) |>
  #create a column indicating if its a KRat
  mutate(is_krat = genus == "Dipodomys") |>
  #count them
  count(is_krat)

glimpse(species)
