library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(deeplr)


# read data ---------------------------------------------------------------

montreal_cd_ct <- sf::st_read("data/report/survey_filter_inside_cd.gpkg")

# read key ----------------------------------------------------------------
key <- data.table::fread("data/translate_key.csv", header = F) %>% 
  purrr::as_vector()


# translate ---------------------------------------------------------------


# * q45_21 ----------------------------------------------------------------

# DONE
# dt_q45_21 <- montreal_cd_ct
# data.table::setDT(dt_q45_21)
# dt_q45_21[
#   !is.na(q45_21_text) & q45_21_text != "-99"
#   , q45_21_trans := deeplr::translate2(
#     text = q45_21_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
#   ]
# 
# dt_q45_21[!is.na(q45_21_text) & q45_21_text != "-99", .(v1,q45_21_text, q45_21_trans)] %>% View()
# 

# * q21_14_text --------------------------------------------------------------


# * q25_29_text -----------------------------------------------------------


# * q104 ------------------------------------------------------------------



# save data ---------------------------------------------------------------

dt_q45_21 <- dt_q45_21 %>% 
  select(v1, q45_21_text, q45_21_trans)
data.table::fwrite(dt_q45_21, "data/translated_q45_21.csv")
