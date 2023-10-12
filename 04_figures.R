
library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(cancensus)
library(fs)
library(hrbrthemes)
library(ggmap)
library(raster)
library(BAMMtools)
library(tmap)
library(DataExplorer)
library(patchwork)
library(mapview)
library(conflicted)
library(stars)
library(showtext)


# remove conflict
select <- dplyr::select

conflicts_prefer(dplyr::filter)

# use different fonts
font_add_google("Roboto", "roboto")
showtext::showtext_auto()

# cancencus ---------------------------------------------------------------

# Set api key
census_key <- data.table::fread("data/census_api_key.csv", header = F)
census_key <- census_key %>% as_vector()
options(cancensus.api_key = census_key)

# Set up cache
dir_create("./cache")
options(cancensus.cache_path = "./cache")

montreal_cma <- get_census(
  # 2021 census
  dataset = 'CA21',

  # toronto is CMA code 535; oshawa 532
  regions = list(
    CMA = c("24462") # metro -> necessary for plotting all the data
    # CD = c("2466") # regional district
    # CSD = c("2466023") # city
    ),

  # our variables of interest
  vectors = c(
  ),

  # census DA geography
  # level = 'CT',

  # put the downloaded data in our cache directory
  use_cache = TRUE,

  # get the associated geography in {sf} format
  geo_format = 'sf'
) %>% janitor::clean_names()

# save mtl cma
sf::st_write(montreal_cma, "data/montreal_cma_unified.gpkg", append = F)

montreal_cd <- get_census(
  # 2021 census
  dataset = 'CA21',
  
  # toronto is CMA code 535; oshawa 532
  regions = list(
    # CMA = c("24462") # metro -> necessary for plotting all the data
    CD = c("2466") # regional district
    # CSD = c("2466023") # city
  ),
  
  # our variables of interest
  vectors = c(
  ),
  
  # census DA geography
  # level = 'CT',
  
  # put the downloaded data in our cache directory
  use_cache = TRUE,
  
  # get the associated geography in {sf} format
  geo_format = 'sf'
) %>% janitor::clean_names()

# save mtl cma
sf::st_write(montreal_cd, "data/montreal_cd_unified.gpkg", append = F)

# read data ---------------------------------------------------------------
montreal_cd_ct <- sf::st_read("data/report/montreal_cd_ct_count.gpkg")
montreal_fsa <- sf::st_read("data/report/montreal_fsa_count.gpkg")
survey_filter_cd <- sf::st_read("data/report/survey_filter_inside_cd.gpkg")
load("data/list_count_trips.RData")
montreal_split <- sf::st_read("data/montreal_split/RA_CBC_EOD18.shp") %>% 
  janitor::clean_names()


# crs ---------------------------------------------------------------------
# set all crs to 4326 (bounding box)
montreal_cd_ct <- st_transform(montreal_cd_ct, 4326)
montreal_cma <- st_transform(montreal_cma, 4326)
montreal_fsa <- st_transform(montreal_fsa, 4326)
montreal_split <- st_transform(montreal_split, 4326)


# spatial distribution ----------------------------------------------------


# * bbox ------------------------------------------------------------------

f_bbox_map <- function(sf, zoom_var){
  
  sbox <- st_bbox(sf)
  
  height <- sbox["ymax"] - sbox["ymin"]
  width <- sbox["xmax"] - sbox["xmin"]
  
  border <- c(
    "bottom" = sbox[["ymin"]] - (0.025 * height)
    , "top" = sbox[["ymax"]] + (0.025 * height)
    , "left" = sbox[['xmin']] - (0.025 * width)
    , "right" = sbox[["xmax"]] + (0.025 * width)
  )
  
  names(border) <- c("bottom","top","left","right")
  
  sc_map <- ggmap::get_stamenmap(border, zoom = zoom_var, maptype = "toner-lite")
  
  return(sc_map)
}



# ** cma ------------------------------------------------------------------

sc_map_cma <- f_bbox_map(montreal_cma, 10L)
# save scmap
save(sc_map_cma, file = "data/basemap/sc_map_cma.RData")


# ** cd -------------------------------------------------------------------

sc_map_cd <- f_bbox_map(montreal_cd_ct, 10L)
save(sc_map_cd, file = "data/basemap/sc_map_cd.RData")


# LETS DO THIS ------------------------------------------------------------

# * representation --------------------------------------------------------



# ** spatial distribution --------------------------------------------------


# *** in lim --------------------------------------------------------------

fsa <- montreal_fsa %>% 
  select(geouid) %>% 
  rename("fsa_geouid" = "geouid")
ct <- montreal_cd_ct %>% 
  select(geo_uid, count_lim_tot_parent, count_lim_tot_in_lim) %>% 
  rename("ct_geo_uid" = "geo_uid")

fsa_ct_join <- sf::st_join(fsa, ct)

total_lim_sum <- sum(montreal_cd_ct$count_lim_tot_parent)

fsa_ct_grouped <- fsa_ct_join %>% 
  group_by(fsa_geouid) %>% 
  summarise(
    sum_lim_tot_parent = sum(count_lim_tot_parent, na.rm = T)
    , sum_lim_tot_in_lim = sum(count_lim_tot_in_lim, na.rm = T)
    , prop_tot_pop = sum_lim_tot_parent / total_lim_sum
    , prop_tot_in_lim = sum_lim_tot_in_lim / total_lim_sum
  )

map_pop_dist <- tm_shape(fsa_ct_grouped) +
  tm_polygons(
    col = "prop_tot_pop"
    , style = "jenks"
    # , breaks = c(0, 0.007, 0.011, 0.022, 0.03, 0.04, 0.054, Inf)
    , palette = "YlGnBu"
    , border.alpha = 0.5
    , alpha = 0.95
    , title = "Proportion of Residents (relative to total population)"
  ) + 
  tm_layout(
    legend.width = 2
    # , main.title = "Ratio of the proportions of survey respondents vs. the population over 50 in each CT"
    # , main.title.position = "center"
    # , main.title.size = 1.05
    , legend.title.size = 1.5
    , legend.text.size = 1
    , fontfamily = "roboto"
  ) +
  tm_compass(color.dark = "grey30", text.color = "grey30") +
  tm_scale_bar(
    breaks = c(0,2), color.dark = "grey30", text.color = "grey30"
    , position = "left"
    )

map_in_lim <- tm_shape(fsa_ct_grouped) +
  tm_polygons(
    col = "prop_tot_in_lim"
    , style = "jenks"
    , palette = "YlGnBu"
    , border.alpha = 0.5
    , alpha = 0.95
    , title = "Proportion of Residents in LIM (relative to total population)"
  ) + 
  tm_layout(
    legend.width = 2
    # , main.title = "Ratio of the proportions of survey respondents vs. the population over 50 in each CT"
    # , main.title.position = "center"
    # , main.title.size = 1.05
    , legend.title.size = 1.5
    , legend.text.size = 1
    , fontfamily = "roboto"
  ) +
  tm_compass(color.dark = "grey30", text.color = "grey30") +
  tm_scale_bar(
    breaks = c(0,2)
    , color.dark = "grey30"
    , text.color = "grey30"
    , position = "left"
    )

tmap_arrange(map_pop_dist, map_in_lim)


# *** under/over  ---------------------------------------------------------

# basemapmtl <- raster::raster("data/basemap/teste80.tif")
# test_df <- as.data.frame(basemapmtl)
# basemapmtl

# basemapmtl <- stars::read_stars("data/basemap/teste100.tif")

# ggplot() +
#   geom_sf(
#     data = montreal_fsa
#     , aes(geometry=geom, fill = under_over)
#   ) +
#   hrbrthemes::theme_ipsum() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank()
#     # rect = element_blank()
#     , panel.grid.major = element_blank()
#     , panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=1)
#   ) +
#   ggplot2::scale_fill_continuous()



map_under_over <- tm_shape(montreal_fsa) + 
  tm_polygons(
    col = "under_over"
    , breaks = c(0,0.5, 0.99, 1.01, 2, Inf)
    , palette = "RdBu"
    , border.alpha = 0.5
    , alpha = 0.95
    , labels = c("0.00-0.50: Survey underrepresents","0.50-0.99", '0.99-1.01: Similar representation', '1.01-2.00', '2.00 or more: Survey overrepresents')
    , title = "Ratio: Survey/(Census)"
  ) + 
  tm_layout(
    legend.width = 2
    # , main.title = "Ratio of the proportions of survey respondents vs. the population over 50 in each CT"
    # , main.title.position = "center"
    # , main.title.size = 1.05
    , legend.title.size = 1.5
    , legend.text.size = 1
    , fontfamily = "roboto"
  ) +
  tm_compass(color.dark = "grey30", text.color = "grey30") +
  tm_scale_bar(breaks = c(0,2), color.dark = "grey30", text.color = "grey30"
               , position = "left")

tmap_arrange(map_in_lim, map_under_over)

tmap_arrange(map_pop_dist, map_under_over)

# ** census comparison demographic -----------------------------------------

obssurvey <- survey_filter_cd %>% nrow()

census_age_sums <- montreal_cd_ct %>% 
  st_drop_geometry() %>% 
  select(matches("^age_(50_over|50_64|65_over)")) %>% 
  summarise(across(everything(), ~ sum(., na.rm = T)))
  

df_representation <- tibble(
  prop_lim_census = sum(montreal_cd_ct$count_lim_tot_in_lim, na.rm = T) /
    sum(montreal_cd_ct$count_lim_tot_parent, na.rm = T)
  , prop_lim_survey = survey_filter_cd %>% filter(in_lim=="Yes") %>% nrow() /
    obssurvey
  , prop_vis_minority_census = sum(montreal_cd_ct$tot_vis_minority_tot, na.rm = T) /
    sum(montreal_cd_ct$tot_vis_minority_tot_parent, na.rm = T)
  , prop_vis_minority_survey = survey_filter_cd %>% filter(visible_minority=="Visible minority") %>% nrow() /
    obssurvey
  , prop_male_census = sum(montreal_cd_ct$male_tot, na.rm = T) / sum(montreal_cd_ct$total_tot, na.rm = T)
  , prop_male_survey = survey_filter_cd %>% filter(gender=="male") %>% nrow() / obssurvey
  , prop_female_census = sum(montreal_cd_ct$female_tot, na.rm = T) / sum(montreal_cd_ct$total_tot, na.rm = T)
  , prop_female_survey = survey_filter_cd %>% filter(gender=="female") %>% nrow() / obssurvey
  , prop_age_50_64_tot_census = census_age_sums$age_50_64_tot / census_age_sums$age_50_over_tot
  , prop_age_50_64_tot_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Control (50-64)") %>% nrow() / obssurvey
  , prop_age_50_64_mal_census = census_age_sums$age_50_64_mal / census_age_sums$age_50_over_mal
  , prop_age_50_64_mal_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Control (50-64)" & gender=="male") %>% nrow() / survey_filter_cd %>% filter(gender=="male") %>% nrow()
  , prop_age_50_64_fem_census = census_age_sums$age_50_64_fem / census_age_sums$age_50_over_fem
  , prop_age_50_64_fem_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Control (50-64)" & gender=="female") %>% nrow() / survey_filter_cd %>% filter(gender=="female") %>% nrow()
  , prop_age_65_over_tot_census = census_age_sums$age_65_over_tot / census_age_sums$age_50_over_tot
  , prop_age_65_over_tot_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Treatment (65-over)") %>% nrow() / obssurvey
  , prop_age_65_over_mal_census = census_age_sums$age_65_over_mal / census_age_sums$age_50_over_mal
  , prop_age_65_over_mal_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Treatment (65-over)" & gender=="male") %>% nrow() / survey_filter_cd %>% filter(gender=="male") %>% nrow()
  , prop_age_65_over_fem_census = census_age_sums$age_65_over_fem / census_age_sums$age_50_over_fem
  , prop_age_65_over_fem_survey = survey_filter_cd %>% filter(age_groups_control_treat=="Treatment (65-over)" & gender=="female") %>% nrow() / survey_filter_cd %>% filter(gender=="female") %>% nrow()
  
  # , prop_age_65_over_tot_census = census_age_sums$age_65_over_tot / census_age_sums$age_50_over_tot
  # , prop_age_65_over_tot__survey = survey_filter_cd %>% filter(age_groups_control_treat=="Treatment (65-over)") %>% nrow() / obssurvey
) %>% 
  map_df(~round(., 2))



# * descriptive stats -----------------------------------------------------

survey_filter_cd %>% select(
  visible_minority
  , gender
  , age_groups_control_treat
  , age_groups_5
  , lone_household
  , educ_has_postsec
  , employed
  , unemployed
  , employment
  , immigrant
  , any_mobility_limitation
  , any_functional_limitation
  , in_lim
  , has_vehicle_household
) %>% 
  rename(
    "Visible minority" = visible_minority
    , "Gender" = gender
    , 'Control and treatment' = age_groups_control_treat
    , "Age groups" = age_groups_5
    , "Lone household" = lone_household
    , "Has post-education" = educ_has_postsec
    , "Employed" = employed
    , "Unemployed" = unemployed
    , "Employment" = employment
    , "Immigrant" = immigrant
    , "Any mobility limitation" = any_mobility_limitation
    , "Any functional limitation" = any_functional_limitation
    , "In LIM" = in_lim
    , "Has Vehicle in Household" = has_vehicle_household
  ) %>% 
  mutate(
    Gender = fct_recode(Gender, Female = "female", Male = "male", Other = "other")
    , `Age groups` = fct_recode(`Age groups`, `Age 65 and over` = "age_65_over"
                              , `Age 60-64` = "age_60_64"
                              , `Age 55-59` = "age_55_59"
                              , `Age 50-54` = "age_50_54")
  ) %>% 
DataExplorer::plot_bar(
  data = .
  , ggtheme = hrbrthemes::theme_ipsum(grid = "X", base_family = "roboto")
) 



# teste -------------------------------------------------------------------

f_bar_pct <- function(df, var1){
  
  fg <- df %>%
    count({{var1}}) %>%
    mutate(
      perc = round(proportions(n) * 100, 1),
      res = str_c(n, " (", perc, ")%"),
      cyl = as.factor({{var1}})
    )
  
  ggplot(fg, aes({{var1}}, n, fill = {{var1}})) +
    geom_col() +
    geom_text(aes(label = res), vjust = -0.5) +
    hrbrthemes::theme_ipsum(grid = "Y", base_family = "roboto") +
    scale_fill_brewer(palette = "Set2", na.value = "grey")
  
}

# gender
g_gender <- f_bar_pct(survey_filter_cd, gender)

# ctl treatment
g_ctl_tmt <- f_bar_pct(survey_filter_cd, age_groups_control_treat)

survey_filter_cd %>%
  count(age_groups_control_treat) %>%
  mutate(
    perc = round(proportions(n) * 100, 1),
    res = str_c(n, " (", perc, ")%"),
    # cyl = as.factor(age_groups_control_treat)
  ) %>% 
  ggplot(aes(age_groups_control_treat, n, fill = age_groups_control_treat)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5) +
  hrbrthemes::theme_ipsum(grid = "Y", base_family = "roboto", base_size = 14) +
  scale_fill_manual(values = c("Treatment (65-over)" = "#373494", "Control (50-64)" = "#590039", na.value = "grey")) +
  # scale_fill_brewer(palette = "Set2", na.value = "grey") +
  labs(fill = "Control or Treatment", y = "Frequencies", x = "") +
  theme(legend.position = "bottom")


# access cars
g_access_vehicle <- f_bar_pct(survey_filter_cd, has_vehicle_household)

survey_filter_cd %>%
  count(has_vehicle_household) %>%
  mutate(
    perc = round(proportions(n) * 100, 1),
    res = str_c(n, " (", perc, ")%"),
    cyl = as.factor(has_vehicle_household)
  ) %>% 
  ggplot(aes(has_vehicle_household, n, fill = has_vehicle_household)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5) +
  hrbrthemes::theme_ipsum(grid = "Y", base_family = "roboto", base_size = 14) +
  scale_fill_manual(values = c("Yes" = "#373494", "No" = "#590039", na.value = "grey")) +
  # scale_fill_brewer(palette = "Set2", na.value = "grey") +
  labs(fill = "Vehicle in Household", y = "Frequencies (%)", x = "") +
  theme(legend.position = "bottom")

# mobility limitation
g_mobility_limitation <- f_bar_pct(survey_filter_cd, any_mobility_limitation)

survey_filter_cd %>%
  count(any_mobility_limitation) %>%
  mutate(
    perc = round(proportions(n) * 100, 1),
    res = str_c(n, " (", perc, ")%"),
    cyl = as.factor(any_mobility_limitation)
  ) %>% 
  ggplot(aes(any_mobility_limitation, n, fill = any_mobility_limitation)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5) +
  hrbrthemes::theme_ipsum(grid = "Y", base_family = "roboto", base_size = 14) +
  scale_fill_manual(values = c("Yes" = "#373494", "No" = "#590039", na.value = "grey")) +
  # scale_fill_brewer(palette = "Set2", na.value = "grey") +
  labs(fill = "Any mobility limitation", y = "Frequencies", x = "") +
  theme(legend.position = "bottom")

# in lim
g_in_lim <- f_bar_pct(survey_filter_cd, in_lim)

survey_filter_cd %>%
  count(in_lim) %>%
  mutate(
    perc = round(proportions(n) * 100, 1),
    res = str_c(n, " (", perc, ")%"),
    cyl = as.factor(in_lim)
  ) %>% 
  ggplot(aes(in_lim, n, fill = in_lim)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5) +
  hrbrthemes::theme_ipsum(grid = "Y", base_family = "roboto", base_size = 14) +
  scale_fill_manual(values = c("Yes" = "#373494", "No" = "#590039", na.value = "grey")) +
  # scale_fill_brewer(palette = "Set2", na.value = "grey") +
  labs(fill = "Low Income Measure (LIM)", y = "Frequencies", x = "") +
  theme(legend.position = "bottom")



# * travel behaviour ------------------------------------------------------


# ** awareness free pass ---------------------------------------------------

# survey_filter_cd %>% select(
#   visible_minority
#   , gender
#   , age_groups_control_treat
#   , age_groups_5
#   , lone_household
#   , educ_has_postsec
#   , employed
#   , unemployed
#   , employment
#   , immigrant
#   , any_mobility_limitation
#   , any_functional_limitation
#   , in_lim
#   , awereness_free_pas65
# ) %>% 
#   rename(
#     "Visible minority" = visible_minority
#     , "Gender" = gender
#     , 'Control and treatment' = age_groups_control_treat
#     , "Age groups" = age_groups_5
#     , "Lone household" = lone_household
#     , "Has post-education" = educ_has_postsec
#     , "Employed" = employed
#     , "Unemployed" = unemployed
#     , "Employment" = employment
#     , "Immigrant" = immigrant
#     , "Any mobility limitation" = any_mobility_limitation
#     , "Any functional limitation" = any_functional_limitation
#     , "In LIM" = in_lim
#   ) %>% 
#   mutate(
#     Gender = fct_recode(Gender, Female = "female", Male = "male", Other = "other")
#     , `Age groups` = fct_recode(`Age groups`, `Age 65 and over` = "age_65_over"
#                                 , `Age 60-64` = "age_60_64"
#                                 , `Age 55-59` = "age_55_59"
#                                 , `Age 50-54` = "age_50_54")
#   ) %>% 
#   DataExplorer::plot_bar(
#     data = .
#     , by = "awereness_free_pas65"
#     , by_position = "dodge"
#     , ggtheme = hrbrthemes::theme_ipsum(grid = "X")
#   ) 

a <- survey_filter_cd %>% 
  st_drop_geometry() %>% 
  group_by(in_lim) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n, na.rm = T)) 

a %>% ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = in_lim, y = n))


# ** cross tabs -----------------------------------------------------------

# count

survey_filter_cd %>% select(
  # visible_minority
  , gender
  , age_groups_control_treat
  # , age_groups_5
  # , lone_household
  # , educ_has_postsec
  # , employed
  # , unemployed
  # , employment
  # , immigrant
  , any_mobility_limitation
  # , any_functional_limitation
  , in_lim
  , has_vehicle_household
  , satisfaction_activity_participation
) %>% 
  rename(
    # "Visible minority" = visible_minority
    , "Gender" = gender
    , 'Control and treatment' = age_groups_control_treat
    # , "Age groups" = age_groups_5
    # , "Lone household" = lone_household
    # , "Has post-education" = educ_has_postsec
    # , "Employed" = employed
    # , "Unemployed" = unemployed
    # , "Employment" = employment
    # , "Immigrant" = immigrant
    , "Any mobility limitation" = any_mobility_limitation
    # , "Any functional limitation" = any_functional_limitation
    , "In LIM" = in_lim
    , "Has Vehicle in Household" = has_vehicle_household
    # ,satisfaction_activity_participation
  ) %>% 
  mutate(
    Gender = fct_recode(Gender, Female = "female", Male = "male", Other = "other")
    # , `Age groups` = fct_recode(`Age groups`, `Age 65 and over` = "age_65_over"
    #                             , `Age 60-64` = "age_60_64"
    #                             , `Age 55-59` = "age_55_59"
    #                             , `Age 50-54` = "age_50_54")
  ) %>%
  DataExplorer::plot_bar(
    data = .
    , by = "satisfaction_activity_participation"
    # , by_position = "dodge"
    , ggtheme = hrbrthemes::theme_ipsum(grid = "X", base_family = "roboto")
    , 
  ) 



# ** perceived barriers ---------------------------------------------------

survey_filter_cd %>% select(
  # visible_minority
  , gender
  , age_groups_control_treat
  # , age_groups_5
  # , lone_household
  # , educ_has_postsec
  # , employed
  # , unemployed
  # , employment
  # , immigrant
  , any_mobility_limitation
  # , any_functional_limitation
  , in_lim
  , has_vehicle_household
  , satisfaction_activity_participation
  , has_cost_prevented_transit
) %>% 
  rename(
    # "Visible minority" = visible_minority
    , "Gender" = gender
    , 'Control and treatment' = age_groups_control_treat
    # , "Age groups" = age_groups_5
    # , "Lone household" = lone_household
    # , "Has post-education" = educ_has_postsec
    # , "Employed" = employed
    # , "Unemployed" = unemployed
    # , "Employment" = employment
    # , "Immigrant" = immigrant
    , "Any mobility limitation" = any_mobility_limitation
    # , "Any functional limitation" = any_functional_limitation
    , "In LIM" = in_lim
    , "Household Vehicle" = has_vehicle_household
    , "Activity.participation" = satisfaction_activity_participation
    , "Cost.prevented.transit" = has_cost_prevented_transit
  ) %>% 
  mutate(
    Gender = fct_recode(Gender, Female = "female", Male = "male", Other = "other")
    # , `Age groups` = fct_recode(`Age groups`, `Age 65 and over` = "age_65_over"
    #                             , `Age 60-64` = "age_60_64"
    #                             , `Age 55-59` = "age_55_59"
    #                             , `Age 50-54` = "age_50_54")
  ) %>%
  DataExplorer::plot_bar(
    data = .
    , by = "Cost.prevented.transit"
    # , by_position = "dodge"
    , ggtheme = hrbrthemes::theme_ipsum(grid = "X", base_family = "roboto")
  ) 
