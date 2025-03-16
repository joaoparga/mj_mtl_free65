
# library -----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(formattable)
library(openxlsx2)
library(stringr)

# read data ---------------------------------------------------------------
df_lim <- openxlsx2::read_xlsx("data/statscan_lim_threshold/statscan_1110023201_limtable_onlydata.xlsx")

# format data -------------------------------------------------------------

df_lim <- df_lim %>% 
  pivot_longer(cols = `2018`:`2022`, names_to = "year") %>% 
  rename("household_size" = household_size, "lim_cutoff" = value)

df_lim <- df_lim %>% 
  mutate(year = as.integer(year))

# filter 2022 only
df_lim <- df_lim %>% 
  filter(year==2022L)

# extract only digits from household size
df_lim <- df_lim %>% 
  mutate(household_size = str_extract(
    string = household_size 
    , pattern = "[[:digit:]]{1,2}"
    ) %>% as.integer()
    )

# save data ---------------------------------------------------------------
save(df_lim, file = "data/statscan_lim_threshold/statscan_limthreshold_formatted.RData")
# openxlsx2::write_xlsx(df_lim, "data/statscan_lim_threshold/statscan_limthreshold_formatted.xlsx")
