
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


# read key ----------------------------------------------------------------
key <- data.table::fread("data/translate_key.csv", header = F) %>% 
  purrr::as_vector()


# check key usage ---------------------------------------------------------

deeplr::usage2(key)

# translate ---------------------------------------------------------------


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
#              .(v1,v2_q45_21_text, v2_q45_21_trans)] %>% View()


# ** save data ------------------------------------------------------------

# dt_v2_q45_21 <- dt_v2_q45_21 %>%
#   select(v1, v2_q45_21_text, v2_q45_21_trans)
# data.table::fwrite(dt_v2_q45_21, "data/translated_v2_q45_21.csv")

# * q21_14_text --------------------------------------------------------------

# DONE

# dt_v2_q21_14_text <- df_wave2
# data.table::setDT(dt_v2_q21_14_text)
# 
# dt_v2_q21_14_text[
#   !is.na(v2_q21_14_text) & v2_q21_14_text != "-99"
#   , v2_q21_14_text_trans := deeplr::translate2(
#     text = v2_q21_14_text, target_lang = "EN", source_lang = "FR", auth_key = key
#   )
#   ]

# dt_v2_q21_14_text[!is.na(v2_q21_14_text) & v2_q21_14_text != "-99", 
#                   .(v1, v2_q21_14_text, v2_q21_14_text_trans)] %>% View()


# ** save data ------------------------------------------------------------

# dt_v2_q21_14_text <- dt_v2_q21_14_text %>% 
#   select(v1, v2_q21_14_text, v2_q21_14_text_trans)
# data.table::fwrite(dt_v2_q21_14_text, "data/translated_v2_q21_14_text.csv")

# * q25_29_text -----------------------------------------------------------


# * q104 ------------------------------------------------------------------


# * q42_10_text -----------------------------------------------------------


# * q45_21_text -----------------------------------------------------------




