
# library -----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(hrbrthemes)


# read data ---------------------------------------------------------------

load(file = "data/df_both_waves.RData")


# clean time --------------------------------------------------------------

df_first <- df_both %>% 
  # columns
  select(wave, start_date, end_date) %>% 
  # filter na either on end or start date
    filter(!is.na(start_date) | 
             !is.na(end_date)
             ) %>% 
  filter(wave == "First")

# parse columns to get time information
df_first <- df_first %>% 
  mutate(
    start_time = ymd_hm(start_date)
    , end_time = ymd_hm(end_date)
  )

# survey answering time
# base R
df_first <- df_first %>% 
  mutate(
    survey_length_minutes = difftime(end_time, start_time, units = "mins")
    # , survey_length_min = end_time - start_time
  )
  



# descriptive stats -------------------------------------------------------

df_first %>% 
  filter(survey_length_minutes >= 5 & survey_length_minutes < 120) %>% 
  pull(survey_length_minutes) %>% mean()

df_first %>% 
  filter(survey_length_minutes >= 5 & survey_length_minutes < 120) %>% 
  pull(survey_length_minutes) %>% median()

# plot --------------------------------------------------------------------

df_first %>% 
  filter(survey_length_minutes < 120) %>% 
  ggplot(
     aes(x = survey_length_minutes)
  ) +
  geom_boxplot() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25, seq(50,100,50)), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_ipsum()
  
  
df_first %>% 
  filter(survey_length_minutes < 120) %>% 
  ggplot(
    aes(x = survey_length_minutes)
  ) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25, seq(50,100,50)), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_ipsum()
