
# description -------------------------------------------------------------

# this code translates questions only form the second wave 

# libraries ---------------------------------------------------------------


library(tidyverse)
library(sf)
library(data.table)
library(janitor)
library(deeplr)


# read data ---------------------------------------------------------------

df_wave2 <- data.table::fread("data/wave2_unified_data/JoinedData_17_11_2023_raw.csv")
df_wave2 <-  janitor::clean_names(df_wave2)


# remove duplicate --------------------------------------------------------

# check 05_2_wave2_create_vars.qmd for explanation on why remove this row
df_wave2 <- df_wave2 %>% 
  filter(!v1==1424)

# read key ----------------------------------------------------------------
key <- data.table::fread("data/translate_key.csv", header = F) %>% 
  purrr::as_vector()


# check key usage ---------------------------------------------------------

deeplr::usage2(key)

# translate ---------------------------------------------------------------


# OLD VARS ----------------------------------------------------------------
survey_filter_cd <- sf::st_read("data/report/survey_filter_inside_cd.gpkg")
# 
# dt_q45_21 <- survey_filter_cd
# data.table::setDT(dt_q45_21)
# dt_q45_21[
#   !is.na(q45_21_text) & q45_21_text != "-99"
#   , q45_21_trans := deeplr::translate2(
#     text = q45_21_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
  # ]

# dt_q45_21[!is.na(q45_21_text) & q45_21_text != "-99", .(response_id,q45_21_text, q45_21_trans)] %>% View()


# dt_q45_21 <- dt_q45_21 %>%
#   select(response_id, q45_21_text, q45_21_trans)
# data.table::fwrite(dt_q45_21, "data/translated_q45_21_newidvar.csv")


# NEW VARS ----------------------------------------------------------------

dt_q21_14_text <- survey_filter_cd
data.table::setDT(dt_q21_14_text)

dt_q21_14_text[
  !is.na(q21_14_text) & q21_14_text != "-99"
  , q21_14_trans := deeplr::translate2(
    text = q21_14_text, target_lang = "EN", source_lang = "FR", auth_key = key
  )
]

dt_q21_14_text[
  !is.na(q21_14_text) & q21_14_text != "-99", 
  .(response_id, q21_14_text, q21_14_trans)] %>% View()



# ** save data ------------------------------------------------------------

dt_q21_14_text <- dt_q21_14_text %>% 
  select(response_id, q21_14_text, q21_14_trans)
data.table::fwrite(dt_q21_14_text, "data/translated_q21_14_text_newidvar.csv")

# * q45_21 ----------------------------------------------------------------

# DONE
# dt_v2_q45_21 <- df_wave2
# data.table::setDT(dt_v2_q45_21)
# dt_v2_q45_21[
#   !is.na(v2_q45_21_text) & v2_q45_21_text != "-99"
#   , v2_q45_21_trans := deeplr::translate2(
#     text = v2_q45_21_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
#   ]

# dt_v2_q45_21[!is.na(v2_q45_21_text) & v2_q45_21_text != "-99",
#              .(response_id,v2_q45_21_text, v2_q45_21_trans)] %>% View()


# ** save data ------------------------------------------------------------

# dt_v2_q45_21 <- dt_v2_q45_21 %>%
#   select(response_id, v2_q45_21_text, v2_q45_21_trans)
# data.table::fwrite(dt_v2_q45_21, "data/translated_v2_q45_21.csv")

# * q21_14_text --------------------------------------------------------------

# DONE

# dt_v2_q21_14_text <- df_wave2
# data.table::setDT(dt_v2_q21_14_text)
# 
# dt_v2_q21_14_text[
#   !is.na(v2_q21_14_text) & v2_q21_14_text != "-99"
#   , v2_q21_14_trans := deeplr::translate2(
#     text = v2_q21_14_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
#   ]

# dt_v2_q21_14_text[!is.na(v2_q21_14_text) & v2_q21_14_text != "-99",
#                   .(response_id, v2_q21_14_text, v2_q21_14_trans)] %>% View()



# * q112 ------------------------------------------------------------------


# * q114 ------------------------------------------------------------------

# ** save data ------------------------------------------------------------

# dt_v2_q21_14_text <- dt_v2_q21_14_text %>%
#   select(response_id, v2_q21_14_text, v2_q21_14_trans)
# data.table::fwrite(dt_v2_q21_14_text, "data/translated_v2_q21_14_text.csv")

# * q25_29_text -----------------------------------------------------------


# * q104 ------------------------------------------------------------------


# * q42_10_text -----------------------------------------------------------


# * q45_21_text -----------------------------------------------------------




