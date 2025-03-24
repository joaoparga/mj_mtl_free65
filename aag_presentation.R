
# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(data.table)
library(tmap)

# read data ---------------------------------------------------------------


# * mtl survey data -------------------------------------------------------

load(file="data/df_both_outliers.RData")
load(file="data/l_outcomes.RData")


# * MTL cd data -----------------------------------------------------------

mtl_cd <- sf::read_sf("data/report/montreal_cd_ct_count.gpkg")


# * mtl transit lines -----------------------------------------------------

stm_lines <- sf::read_sf("data/stm_sig/stm_lignes_sig.shp")

stm_stops <- sf::read_sf("data/stm_sig/stm_arrets_sig.shp")


# plot1 -------------------------------------------------------------------

# POPULATION DISTRIBUTION

mtl_cd %>% 
  ggplot() + 
  geom_sf(aes(fill = population_2016), colour = NA) + 
  geom_sf(data = stm_lines, aes()) +  theme_void() + scale_fill_viridis_c()

# plot2 -------------------------------------------------------------------


