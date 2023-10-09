library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(deeplr)


# read data ---------------------------------------------------------------

survey_filter_cd <- sf::st_read("data/report/survey_filter_inside_cd.gpkg")

# read key ----------------------------------------------------------------
key <- data.table::fread("data/translate_key.csv", header = F) %>% 
  purrr::as_vector()


# check key usage ---------------------------------------------------------

deeplr::usage2(key)

# translate ---------------------------------------------------------------


# * q45_21 ----------------------------------------------------------------

# DONE
# dt_q45_21 <- survey_filter_cd
# data.table::setDT(dt_q45_21)
# dt_q45_21[
#   !is.na(q45_21_text) & q45_21_text != "-99"
#   , q45_21_trans := deeplr::translate2(
#     text = q45_21_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
#   ]
# 
# dt_q45_21[!is.na(q45_21_text) & q45_21_text != "-99", .(v1,q45_21_text, q45_21_trans)] %>% View()


# ** save data ------------------------------------------------------------

# dt_q45_21 <- dt_q45_21 %>% 
#   select(v1, q45_21_text, q45_21_trans)
# data.table::fwrite(dt_q45_21, "data/translated_q45_21.csv")

# * q21_14_text --------------------------------------------------------------
dt_q21_14_text <- survey_filter_cd
data.table::setDT(dt_q21_14_text)

dt_q21_14_text[
  !is.na(q21_14_text) & q21_14_text != "-99"
  , q21_14_text_trans := deeplr::translate2(
    text = q21_14_text, target_lang = "EN", source_lang = "FR", auth_key = key
  )
  ]

# dt_q21_14_text[!is.na(q21_14_text) & q21_14_text != "-99", .(v1,q45_21_text, q45_21_trans)] %>% View()


# ** save data ------------------------------------------------------------

dt_q21_14_text <- dt_q21_14_text %>% 
  select(v1, q21_14_text, q21_14_text_trans)
data.table::fwrite(dt_q21_14_text, "data/translated_q21_14_text.csv")

# * q25_29_text -----------------------------------------------------------


# * q104 ------------------------------------------------------------------


# * q42_10_text -----------------------------------------------------------


# * q45_21_text -----------------------------------------------------------




