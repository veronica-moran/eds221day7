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
    Year = as.numeric(str_sub(Date, 1L, 4L))
  )

glimpse(coral_depth_Y)

coral_quadrat <- coral_depth_Y |>
  summarize(
    quadrat_cover = sum(Percent_Cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth, Quad40)
  )
mean_coral_cover <- coral_quadrat |>
  summarize(
    mean_coral_cover = mean(quadrat_cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth)
  )

coral_summary <- mean_coral_cover |>
  arrange(Year, Site, Depth)


primary_consumer <- moorea_fish |>
  filter(Coarse_Trophic == "Primary Consumer")


fish_summary <- primary_consumer |>
  summarize(
    total_biomass = sum(Biomass, na.rm = TRUE),
    .by = c(Site, Habitat, Year)
  )

reef_joined <- inner_join(
  coral_summary,
  fish_summary,
  by = c("Site", "Habitat", "Year")
)


nrow(coral_summary)
nrow(fish_summary)
nrow(reef_joined)


reef_wide <- reef_joined |>
  select(Site, Habitat, Year, mean_coral_cover) |>
  pivot_wider(
    names_from = Habitat,
    values_from = mean_coral_cover
  )

reef_wide <- reef_wide |>
  mutate(
    cover_difference = Fringing - Forereef
  )


glimpse(reef_wide)

ggplot(
  data = reef_wide,
  mapping = aes(x = cover_difference)
) +
  geom_histogram()


ggplot(
  data = reef_joined,
  mapping = aes(
    x = mean_coral_cover,
    y = total_biomass
  )
) +
  geom_point() +
  labs(
    x = "Mean Coral Cover",
    y = "Herbivorous Fish Biomass"
  )
