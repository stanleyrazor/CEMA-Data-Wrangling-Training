
# libs'
library(pacman)
p_load(dplyr, ggplot2, sf, leaflet, stringr, janitor)

# data
d1 <- read.csv('https://raw.githubusercontent.com/cema-uonbi/L4H_sample_data/main/table6_teenpregnancybycounty.csv') |>
  clean_names()

# percentage of teenagers who have ever been pregnant by county
