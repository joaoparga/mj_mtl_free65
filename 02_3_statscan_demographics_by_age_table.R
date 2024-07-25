
# setup -------------------------------------------------------------------
library(tidyverse)
library(data.table)
# devtools::install_github("warint/statcanR")
library(statcanR)
library(janitor)

# download data -----------------------------------------------------------
mydata <- statcan_download_data("98-10-0352-01", "eng") %>% 
  janitor::clean_names()

mydata <- mydata %>% 
  select(-c(starts_with("symbol"), ref_date, dguid,coordinate, coordinate_2))

# manipulation ------------------------------------------------------------

# filter ------------------------------------------------------------------

# select montreal
df_mtl <- mydata[grepl("Montréal \\(2466\\)", mydata$geo),]

# age 45 and up
total_45_over <- df_mtl %>% 
  filter(
    statistics_2 == "2021 Counts" &
      age_15c %in% c("45 to 54 years",
                     "55 to 64 years",
                     "65 to 74 years",
                     "75 years and over") &
      gender_3 == "Total - Gender"
  ) %>% 
  summarise(sum(visible_minority_15_total_visible_minority_1, na.rm = T)) %>% 
  as_vector() %>% 
  unname()


total_45_over_vis_min <- df_mtl %>% 
  filter(
    statistics_2 == "2021 Counts" &
      age_15c %in% c("45 to 54 years",
                     "55 to 64 years",
                     "65 to 74 years",
                     "75 years and over") &
      gender_3 == "Total - Gender"
  ) %>% 
  summarise(sum(visible_minority_15_total_visible_minority_population_2, na.rm = T)) %>% 
  as_vector() %>% 
  unname()


# age 55 and up
total_55_over <- df_mtl %>% 
  filter(
    statistics_2 == "2021 Counts" &
      age_15c %in% c("55 to 64 years",
                     "65 to 74 years",
                     "75 years and over") &
      gender_3 == "Total - Gender"
  ) %>% 
  summarise(sum(visible_minority_15_total_visible_minority_1, na.rm = T)) %>% 
  as_vector() %>% 
  unname()


total_55_over_vis_min <- df_mtl %>% 
  filter(
    statistics_2 == "2021 Counts" &
      age_15c %in% c("55 to 64 years",
                     "65 to 74 years",
                     "75 years and over") &
      gender_3 == "Total - Gender"
  ) %>% 
  summarise(sum(visible_minority_15_total_visible_minority_population_2, na.rm = T)) %>% 
  as_vector() %>% 
  unname()


# create df ---------------------------------------------------------------

df_vis_min <- dplyr::tibble(
  total_45_over = total_45_over
  , total_45_over_vis_min = total_45_over_vis_min
  , total_55_over = total_55_over
  , total_55_over_vis_min
) %>% 
  mutate(
    prop_vis_min_45_over = total_45_over_vis_min / total_45_over
    , prop_vis_min_55_over = total_55_over_vis_min / total_55_over
  )


# include gender? ---------------------------------------------------------



# save data ---------------------------------------------------------------

data.table::fwrite(df_vis_min, "data/df_prop_vis_minority_by_age_statscan.csv")
