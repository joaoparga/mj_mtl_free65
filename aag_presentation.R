
# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(data.table)
library(tmap)
library(hrbrthemes)
library(ggstatsplot)
library(ggsignif)
library(patchwork)
library(ggrepel)
library(ggspatial)

# fonts -------------------------------------------------------------------

sysfonts::font_add_google("Roboto", "roboto")
showtext::showtext_auto()

# read data ---------------------------------------------------------------


# * mtl survey data -------------------------------------------------------

load(file="data/df_both_outliers.RData")
load(file="data/l_outcomes.RData")


# * MTL cd data -----------------------------------------------------------

mtl_cd <- sf::read_sf("data/report/montreal_cd_ct_count.gpkg")


# * mtl transit lines -----------------------------------------------------

stm_lines <- sf::read_sf("data/stm_sig/stm_lignes_sig.shp")

stm_stops <- sf::read_sf("data/stm_sig/stm_arrets_sig.shp")



# new vars ----------------------------------------------------------------


# * new_treat wave --------------------------------------------------------


df_both <- df_both %>% 
  mutate(
    new_treat = case_when(
      treatment_50_59 == "Control" |
        treatment_60_64 == "Control" ~ "Control"
      , treatment == "Treatment" ~ "Treatment"
      , .default = NA_character_
    )
    , new_wave = case_when(
      wave == "First" ~ "Before"
      , .default = "After"
    )
  ) %>% 
  mutate(
    new_wave = factor(
      new_wave
      , levels = c("Before", "After")
      , ordered = T
    )
  ) 

# * retired -----------------------------------------------------------------

df_both <- df_both %>% 
  mutate(
    is_retired = case_when(
      employment == "Retired" ~ "Retired"
      , is.na(employment) ~ NA_character_
      , .default = "Not retired"
    )
  )

df_both <- df_both %>% 
  mutate(
    is_retired = factor(
      x = is_retired
      , levels = c("Not retired", "Retired")
    )
  )

# * treatment_60_65plus ---------------------------------------------------


df_both <- df_both %>% 
  mutate(
    treatment_60_65plus = case_when(
      treatment_60_65plus == "Treatment" ~ "Treatment (65+ yrs old & Free Pass)"
      , treatment_60_65plus == "Control" ~ "Control (60+ yrs old & No Free Pass)"
      , .default = NA_character_
    )
  ) %>% 
  mutate(
    treatment_60_65plus = factor(
      x = treatment_60_65plus
      , levels = c("Control (60+ yrs old & No Free Pass)", 
                   "Treatment (65+ yrs old & Free Pass)")
      , ordered = T
    )
  )

# filters -----------------------------------------------------------------

# Filter by control/treatment

66666
Remember: always include filter(!is.na(treatment_60_65plus)) %>% pipe before operations
66666


# plot results -------------------------------------------------------------------


# * p avg daily trips BEFORE/AFTER -----------------------------------------------------

# https://stackoverflow.com/a/67918028

# ** treatment ------------------------------------------------------------

(p_avg_treat <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
    ) %>% 
  ggstatsplot::ggbetweenstats(
  x = new_wave
  , y = avg_trips_daily_wave
  , type = "nonparametric"
  , pairwise.display = "significant"
  , xlab = ""
  , ylab = ""
  , title = "Treatment (65+ yrs old & Free Pass)"
  , subtitle = "Average daily trips per wave"
  , results.subtitle = F
  # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Before", "After")),
    map_signif_level = TRUE
    , family = "roboto"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#f1a340", "#998ec3")
  )
)


# ** control --------------------------------------------------------------

(p_avg_control <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Control (60+ yrs old & No Free Pass)"
  ) %>% 
  ggstatsplot::ggbetweenstats(
    x = new_wave
    , y = avg_trips_daily_wave
    , type = "nonparametric"
    , pairwise.display = "significant"
    , xlab = ""
    , ylab = ""
    , title = "Control (60+ yrs old & No Free Pass)"
    , subtitle = "Average daily trips per wave"
    , results.subtitle = F
    # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Before", "After")),
    map_signif_level = TRUE
    , family = "roboto"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#f1a340", "#998ec3")
  )
)



# ** retired treat --------------------------------------------------------------

(p_avg_retired_treat <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggstatsplot::ggbetweenstats(
    x = is_retired
    , y = avg_trips_daily_wave
    , type = "nonparametric"
    , pairwise.display = "significant"
    , xlab = ""
    , ylab = ""
    , title = "Treatment - Retirement status"
    , subtitle = "Average daily trips per wave"
    , results.subtitle = F
    # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Not retired", "Retired")),
    map_signif_level = TRUE
    , family = "roboto"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#e9a3c9", "#a1d76a")
  )
)

# ** retired control --------------------------------------------------------------


(p_avg_retired_control <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Control (60+ yrs old & No Free Pass)"
  ) %>% 
  ggstatsplot::ggbetweenstats(
    x = is_retired
    , y = avg_trips_daily_wave
    , type = "nonparametric"
    , pairwise.display = "significant"
    , xlab = ""
    , ylab = ""
    , title = "Control - Retirement status"
    , subtitle = "Average daily trips per wave"
    , results.subtitle = F
    # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Not retired", "Retired")),
    map_signif_level = TRUE
    , family = "roboto"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#e9a3c9", "#a1d76a")
  )
)


# * p avg daily trips TREAT/CONTROL -----------------------------------------------------

(p_before <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      new_wave == "Before"
  ) %>% 
  ggstatsplot::ggbetweenstats(
    x = treatment_60_65plus
    , y = avg_trips_daily_wave
    , type = "nonparametric"
    , pairwise.display = "significant"
    , xlab = ""
    , ylab = ""
    , title = "Before intervention"
    , subtitle = "Average daily trips per wave"
    , results.subtitle = F
    # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Control (60+ yrs old & No Free Pass)"
                         , "Treatment (65+ yrs old & Free Pass)")),
    map_signif_level = TRUE
    , family = "roboto"
    , na.rm = T
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#fb2d2d", "#144f67")
  )
)

(p_after <- df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      new_wave == "After"
  ) %>% 
  ggstatsplot::ggbetweenstats(
    x = treatment_60_65plus
    , y = avg_trips_daily_wave
    , type = "nonparametric"
    , pairwise.display = "significant"
    , xlab = ""
    , ylab = ""
    , title = "After intervention"
    , subtitle = "Average daily trips per wave"
    , results.subtitle = F
    # , conf.level = 0.9
  ) +
  geom_signif(
    comparisons = list(c("Control (60+ yrs old & No Free Pass)"
                         , "Treatment (65+ yrs old & Free Pass)")),
    map_signif_level = TRUE
    , family = "roboto"
    , na.rm = T
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto", size = 20),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16)
    ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
    ,axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "none"
    , strip.text.y = element_text(family = "roboto", size = 20)
  ) +
  scale_color_manual(
    values = c("#fb2d2d", "#144f67")
  )
)


# * before/after RETIRED --------------------------------------------------

(p_before_retired <- df_both %>% 
   filter(
     !is.na(treatment_60_65plus) &
       new_wave == "Before"
       # treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
   ) %>% 
   ggstatsplot::ggbetweenstats(
     x = is_retired
     , y = avg_trips_daily_wave
     , type = "nonparametric"
     , pairwise.display = "significant"
     , xlab = ""
     , ylab = ""
     , title = "Before intervention - retirement status"
     , subtitle = "Average daily trips per wave"
     , results.subtitle = F
     # , conf.level = 0.9
   ) +
   geom_signif(
     comparisons = list(c("Not retired", "Retired")),
     map_signif_level = TRUE
     , family = "roboto"
     , na.rm = T
   ) +
   theme_ipsum() +
   theme(
     legend.title=element_blank()
     ,text = element_text(family = "roboto", size = 20)
     , plot.title    = element_text(family = "roboto", size = 20),
     plot.subtitle = element_text(family = "roboto", size = 16),
     axis.title.x  = element_text(family = "roboto", size = 16)
     ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
     ,axis.text.x   = element_text(family = "roboto"),
     axis.text.y   = element_text(family = "roboto")
     , strip.text.x = element_text(family = "roboto", size = 16)
     , legend.position = "none"
     , strip.text.y = element_text(family = "roboto", size = 20)
   ) +
   scale_color_manual(
     values = c("#e9a3c9", "#a1d76a")
   )
)



(p_after_retired <- df_both %>% 
    filter(
      !is.na(treatment_60_65plus) &
        new_wave == "After"
      # treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
    ) %>% 
    ggstatsplot::ggbetweenstats(
      x = is_retired
      , y = avg_trips_daily_wave
      , type = "nonparametric"
      , pairwise.display = "significant"
      , xlab = ""
      , ylab = ""
      , title = "After intervention - retirement status"
      , subtitle = "Average daily trips per wave"
      , results.subtitle = F
      # , conf.level = 0.9
    ) +
    geom_signif(
      comparisons = list(c("Not retired", "Retired")),
      map_signif_level = TRUE
      , family = "roboto"
      , na.rm = T
    ) +
    theme_ipsum() +
    theme(
      legend.title=element_blank()
      ,text = element_text(family = "roboto", size = 20)
      , plot.title    = element_text(family = "roboto", size = 20),
      plot.subtitle = element_text(family = "roboto", size = 16),
      axis.title.x  = element_text(family = "roboto", size = 16)
      ,axis.title.y  = element_text(family = "roboto", size = 16, angle = 0),
      ,axis.text.x   = element_text(family = "roboto"),
      axis.text.y   = element_text(family = "roboto")
      , strip.text.x = element_text(family = "roboto", size = 16)
      , legend.position = "none"
      , strip.text.y = element_text(family = "roboto", size = 20)
    ) +
    scale_color_manual(
      values = c("#e9a3c9", "#a1d76a")
    )
)


# PATCHWORKS --------------------------------------------------------------


# * PATCHWORK CONTROL/TREAT -------------------------------------------------------------

(p_control_treat <-  p_avg_control + p_avg_treat)

(p_control_treat_retired <-  p_avg_retired_control + p_avg_retired_treat
)

# * PATCHWORKS t0 vs t1 ---------------------------------------------------

(p_before_vs_after <- p_before + p_after
)

(p_before_vs_after_retired <- p_before_retired + p_after_retired
)




# line ------------------------------------------------------------------

df_both %>% 
  filter(
    !is.na(treatment_60_65plus)
  ) %>% 
  group_by(new_wave, treatment_60_65plus) %>%
  summarise(
    avg_trips_daily_wave = mean(avg_trips_daily_wave, na.rm = T) %>% 
      round(digits = 2)
    ) %>% 
  ggplot(
    aes(x = new_wave, y = avg_trips_daily_wave, group = treatment_60_65plus
        , color = treatment_60_65plus)
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    aes(fill = treatment_60_65plus)
    # ,shape = 21
    # , colour = "Black"
    , size = 3
  ) +
  ggrepel::geom_text_repel(
    aes(label = avg_trips_daily_wave)
    # , check_overlap = T
    # , nudge_x = position_jitter()
    # , nudge_y = position_jitter()
    , size = 5
    , show.legend = F
  ) + 
  scale_y_continuous(
    limits = c(0,2.75)
    , expand = c(0,0)
    , breaks = c(0, 1, 2, 2.5, 2.75)
      # seq(0, 2.75, 0.5)
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Teste"
    , subtitle = "Average daily trips"
    , x = ""
    , y = ""
  ) +
  scale_colour_manual(
    name = "a"
    , values = c("#fb2d2d", "#144f67")
  ) + 
  scale_fill_manual(
    name = "a"
    , values = c("#fb2d2d", "#144f67")
  ) +
  guides(
    fill = guide_legend(override.aes = list(linetype = 0))
  )





# rides transit as would like -------------------------------------------

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = rides_transit_as_would_like
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Rides transit as would like"
  ) +
  scale_fill_manual(
    # name = "a"
    , values = c("#144f67", "#fb2d2d")
  )

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Control (60+ yrs old & No Free Pass)"
  ) %>% 
  ggbarstats(
    x = rides_transit_as_would_like
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Rides transit as would like"
  ) +
  scale_fill_manual(
    # name = "a"
    , values = c("#144f67", "#fb2d2d")
  )



df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      new_wave == "Before"
      # treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = rides_transit_as_would_like
    , y = treatment_60_65plus
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Rides transit as would like"
  ) +
  scale_fill_manual(
    # name = "a"
    , values = c("#144f67", "#fb2d2d")
  )

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      new_wave == "After"
    # treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = rides_transit_as_would_like
    , y = treatment_60_65plus
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Rides transit as would like"
  ) +
  scale_fill_manual(
    # name = "a"
    , values = c("#144f67", "#fb2d2d")
  )

# * has cost prevented transit --------------------------------------------

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = has_cost_prevented_transit
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Has cost prevented transit use"
  ) +
  scale_fill_manual(
    # name = "a"
    , values = c("#109090", "#ef8383")
  )

# * activity participation ------------------------------------------------


df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = satisfaction_activity_participation
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Satisfaction with activity participation"
  ) +
  scale_fill_manual(
    values = c("#669bbc","#fdf0d5", "#c1121f")
  )

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Control (60+ yrs old & No Free Pass)"
  ) %>% 
  ggbarstats(
    x = satisfaction_activity_participation
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Has cost prevented transit use"
  ) +
  scale_fill_manual(
    values = c("#669bbc","#fdf0d5", "#c1121f")
  )





# ** satisfaction sub -----------------------------------------------------

df_both %>% 
  filter(
    !is.na(treatment_60_65plus) &
      treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)"
  ) %>% 
  ggbarstats(
    x = satisfaction_community_events
    , y = new_wave
    , type = "nonparametric"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
  ) +
  labs(
    # title = "Rides transit as would like"
    # , subtitle = 
    , x = ""
    , y = "Satisfaction with activity participation"
  ) +
  scale_fill_manual(
    values = c("#669bbc","#fdf0d5", "#c1121f")
  )

# plot rdd ------------------------------------------------------------------

# RDD

df_rdd <- df_both %>% 
  filter(!is.na(new_treat) &
           age_corrected >=60L &
           age_corrected<=85L
         # !is.na(treatment_60_65plus)
  ) %>%
  # group_by(age_corrected, new_wave, new_treat) %>%
  # summarise(avg_trips_age = mean(avg_trips_daily_wave, na.rm = T)) %>% 
  mutate(
    age_distance_threshold = case_when(
      is.na(age_corrected) ~ NA_integer_
      , .default = age_corrected - 65L
    )
  )

ggplot() + 
  geom_vline(
    xintercept = 0L
    , linetype = "dotted"
    , alpha = 0.7
    , colour = "antiquewhite4"
    , size = 1
  ) + 
  geom_point(
    data = df_rdd
    , aes(
      x = age_distance_threshold
      , y = avg_trips_daily_wave
      , colour = new_treat
      , group = new_treat
    )
    , alpha = 0.2
    , position = "jitter"
  ) +
  geom_smooth(
    data = df_rdd
    , aes(
      x = age_distance_threshold
      , y = avg_trips_daily_wave
      , colour = new_treat
      , group = new_treat
    )
    , method = "lm"
    , se = F
    , size = 1.25
  ) +
  # WHOLE DATASET (NO DIFF BETWEEN CONTROL VS TREAT)
  geom_smooth(
    data = df_rdd
    , aes(
      x = age_distance_threshold
      , y = avg_trips_daily_wave)
    , colour = "black"
    , se = F
    , method = "lm"
    , size = 1
    , linetype = "dashed"
  ) +
  facet_wrap(~new_wave, scales = "free") +
  
  ylab("") +
  xlab("Distance to 65 years old threshold") +
  labs(
    subtitle = "Average daily trips"
  ) + 
  guides(
    color=guide_legend(override.aes=list(fill=NA))
  ) + 
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    axis.title.x  = element_text(family = "roboto", size = 16),
    axis.title.y  = element_text(family = "roboto", size = 16),
    axis.text.x   = element_text(family = "roboto"),
    axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "bottom"
    , panel.grid.minor.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    # expand = c(0.1,0)
  ) +
  scale_x_continuous(
    breaks = c(-5,0,10,20)
    , limits = c(-6,21)
  ) +
  scale_colour_manual(
    # name = "a"
    , values = c("#fb2d2d", "#144f67")
  ) + 
  scale_fill_manual(
    # name = "a"
    , values = c("#fb2d2d", "#144f67")
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(linetype = 0, alpha = 1)
    )
  )


# # covariate: employed
# 
# df_both %>% 
#   mutate(
#     new_treat = case_when(
#       treatment_50_59 == "Control" |
#         treatment_60_64 == "Control" ~ "Control"
#       , treatment == "Treatment" ~ "Treatment"
#       , .default = NA_character_
#     )
#     , new_wave = case_when(
#       wave == "First" ~ "Before"
#       , .default = "After"
#     )
#   ) %>% 
#   mutate(
#     new_wave = factor(
#       new_wave
#       , levels = c("Before", "After")
#       , ordered = T
#     )
#   ) %>% 
#   filter(!is.na(new_treat) &
#            !is.na(is_retired)
#          # !is.na(treatment_60_65plus)
#   ) %>%
#   group_by(age_corrected, new_wave, new_treat, is_retired) %>%
#   summarise(avg_trips_age = mean(avg_trips_daily_wave, na.rm = T)) %>% 
#   mutate(
#     age_distance_threshold = case_when(
#       is.na(age_corrected) ~ NA_integer_
#       , .default = age_corrected - 65L
#     )
#   )  %>%
#   filter(age_distance_threshold <= 20) %>% 
#   ggplot(
#     aes(
#       x = age_distance_threshold
#       , y = avg_trips_age
#       , colour = new_treat
#       , group = new_treat
#     )
#   ) + 
#   geom_point() +
#   geom_smooth() +
#   geom_vline(
#     xintercept = 0L
#     , linetype = "dashed"
#     , alpha = 0.7
#   ) + 
#   scale_color_manual(
#     values = c("#fb2d2d", "#144f67")
#   ) +
#   facet_grid(rows = vars(is_retired)
#              , cols = vars(new_wave)
#                ) +
#   theme_ipsum()  +
#   ylab("Average daily trips") +
#   xlab("Distance to 65 years old threshold") +
#   guides(
#     color=guide_legend(override.aes=list(fill=NA))
#   ) + 
#   theme(
#     legend.title=element_blank()
#     ,text = element_text(family = "roboto", size = 20)
#     , plot.title    = element_text(family = "roboto"),
#     plot.subtitle = element_text(family = "roboto"),
#     axis.title.x  = element_text(family = "roboto", size = 16),
#     axis.title.y  = element_text(family = "roboto", size = 16),
#     axis.text.x   = element_text(family = "roboto"),
#     axis.text.y   = element_text(family = "roboto")
#     , strip.text.x = element_text(family = "roboto", size = 16)
#     , legend.position = "bottom"
#   )

# plot MAP -------------------------------------------------------------------

# POPULATION DISTRIBUTION
mtl_cd %>% 
  ggplot() + 
  geom_sf(aes(fill = prop_age_65_over_tot), colour = NA) + 
  geom_sf(data = stm_lines, aes()) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis_c(
    name = "Population over 65 years old (%)"
  ) +
  labs(
    title = "Transit lines and Population over 65 years old by CD (%)"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    # axis.title.x  = element_text(family = "roboto", size = 16),
    # axis.title.y  = element_text(family = "roboto", size = 16),
    # axis.text.x   = element_text(family = "roboto"),
    # axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "right"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
  ) 

mtl_cd %>% 
  ggplot() + 
  geom_sf(data = stm_lines, aes()) + 
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "Transit lines"
  ) +
  theme_ipsum() +
  theme(
    legend.title=element_blank()
    ,text = element_text(family = "roboto", size = 20)
    , plot.title    = element_text(family = "roboto"),
    plot.subtitle = element_text(family = "roboto", size = 16),
    # axis.title.x  = element_text(family = "roboto", size = 16),
    # axis.title.y  = element_text(family = "roboto", size = 16),
    # axis.text.x   = element_text(family = "roboto"),
    # axis.text.y   = element_text(family = "roboto")
    , strip.text.x = element_text(family = "roboto", size = 16)
    , legend.position = "right"
    , strip.text.y = element_text(family = "roboto", size = 16)
    , panel.grid.minor.y = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
  ) 




# grouped bar plots --------------------------------------------------------

# * rides transit as would like -------------------------------------------
(p_rides_treat <- df_both %>% 
   filter(
     !is.na(treatment_60_65plus) &
       treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)" &
       !is.na(rides_transit_as_would_like)
   ) %>% 
   ggbarstats(
     x = rides_transit_as_would_like,
     y = new_wave,
     digits.perc = 1,
     # package = "ggsci",
     # palette = "category10_d3",
     # arguments relevant for `combine_plots()`
     # title.text = "Passenger survival on the Titanic by gender and age",
     # caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
     plotgrid.args = list(nrow = 2)
     # , ggtheme = theme_ipsum()
     , type = "nonparametric"
     , pairwise.display = "significant"
     # , xlab = ""
     # , ylab = ""
     , title = "Treatment (65+ yrs old & Free Pass)"
     # , subtitle = "Average daily trips per wave"
     # , results.subtitle = F
     , legend.title = "Rides transit as would like"
     , bf.message = FALSE
     , sample.size.label.args = list(size = 0)
   ) +
   theme_ipsum(
     # legend.title = element_text(family = "roboto", size = 20)
   ) +
   theme(
     legend.title = element_text(family = "roboto", size = 20)
     , text = element_text(family = "roboto", size = 20)
     , plot.title    = element_text(family = "roboto"),
     plot.subtitle = element_text(family = "roboto", size = 14),
     axis.title.x  = element_text(family = "roboto", size = 16),
     axis.title.y  = element_text(family = "roboto", size = 16),
     axis.text.x   = element_text(family = "roboto", size = 16),
     axis.text.y   = element_text(family = "roboto")
     , strip.text.x = element_text(family = "roboto", size = 16)
     , legend.position = "bottom"
     , panel.grid.minor.y = element_blank()
     , panel.grid.minor.x = element_blank()
     , panel.grid.major.x = element_blank()
   ) +
   ylab("Frequency") +
   xlab("") +
   labs(
     # subtitle = "Average daily trips"
   ) +
   scale_fill_manual(
     # name = "a"
     , values = c("#67a9cf", "#ef8a62")
   )
)

(p_rides_control <- df_both %>% 
    filter(
      !is.na(treatment_60_65plus) &
        treatment_60_65plus == "Control (60+ yrs old & No Free Pass)" &
        !is.na(rides_transit_as_would_like)
    ) %>% 
    ggbarstats(
      x = rides_transit_as_would_like,
      y = new_wave,
      digits.perc = 1,
      # package = "ggsci",
      # palette = "category10_d3",
      # arguments relevant for `combine_plots()`
      # title.text = "Passenger survival on the Titanic by gender and age",
      # caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
      plotgrid.args = list(nrow = 2)
      # , ggtheme = theme_ipsum()
      , type = "nonparametric"
      , pairwise.display = "significant"
      # , xlab = ""
      # , ylab = ""
      , title = "Control (60+ yrs old & No Free Pass)"
      # , subtitle = "Average daily trips per wave"
      # , results.subtitle = F
      , legend.title = "Rides transit as would like"
      , bf.message = FALSE
      , sample.size.label.args = list(size = 0)
    ) +
    theme_ipsum(
      # legend.title = element_text(family = "roboto", size = 20)
    ) +
    theme(
      legend.title = element_text(family = "roboto", size = 20)
      , text = element_text(family = "roboto", size = 20)
      , plot.title    = element_text(family = "roboto"),
      plot.subtitle = element_text(family = "roboto", size = 14),
      axis.title.x  = element_text(family = "roboto", size = 16),
      axis.title.y  = element_text(family = "roboto", size = 16),
      axis.text.x   = element_text(family = "roboto", size = 16),
      axis.text.y   = element_text(family = "roboto")
      , strip.text.x = element_text(family = "roboto", size = 16)
      , legend.position = "bottom"
      , panel.grid.minor.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.major.x = element_blank()
      , 
    ) +
    ylab("Frequency") +
    xlab("") +
    labs(
      # subtitle = "Average daily trips"
    ) +
    scale_fill_manual(
      # name = "a"
      , values = c("#67a9cf", "#ef8a62")
    )
)

(p_rides <-  p_rides_control + p_rides_treat +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
    )

# * cost prevented transit --------------------------------------------------

(p_cost_prevented_treat <- df_both %>% 
   filter(
     !is.na(treatment_60_65plus) &
       treatment_60_65plus == "Treatment (65+ yrs old & Free Pass)" &
       !is.na(has_cost_prevented_transit)
   ) %>% 
   ggbarstats(
     x = has_cost_prevented_transit,
     y = new_wave,
     digits.perc = 1,
     # package = "ggsci",
     # palette = "category10_d3",
     # arguments relevant for `combine_plots()`
     # title.text = "Passenger survival on the Titanic by gender and age",
     # caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
     plotgrid.args = list(nrow = 2)
     # , ggtheme = theme_ipsum()
     , type = "nonparametric"
     , pairwise.display = "significant"
     # , xlab = ""
     # , ylab = ""
     , title = "Treatment (65+ yrs old & Free Pass)"
     # , subtitle = "Average daily trips per wave"
     # , results.subtitle = F
     , legend.title = "Has cost prevented transit use"
     , bf.message = FALSE
     , sample.size.label.args = list(size = 0)
   ) +
   theme_ipsum(
     # legend.title = element_text(family = "roboto", size = 20)
   ) +
   theme(
     legend.title = element_text(family = "roboto", size = 20)
     , text = element_text(family = "roboto", size = 20)
     , plot.title    = element_text(family = "roboto"),
     plot.subtitle = element_text(family = "roboto", size = 12),
     axis.title.x  = element_text(family = "roboto", size = 16),
     axis.title.y  = element_text(family = "roboto", size = 16),
     axis.text.x   = element_text(family = "roboto", size = 16),
     axis.text.y   = element_text(family = "roboto")
     , strip.text.x = element_text(family = "roboto", size = 16)
     , legend.position = "bottom"
     , panel.grid.minor.y = element_blank()
     , panel.grid.minor.x = element_blank()
     , panel.grid.major.x = element_blank()
   ) +
   ylab("Frequency") +
   xlab("") +
   labs(
     # subtitle = "Average daily trips"
   ) +
   scale_fill_manual(
     # name = "a"
     , values = c("#67a9cf", "#ef8a62")
   )
)

(p_cost_prevented_control <- df_both %>% 
    filter(
      !is.na(treatment_60_65plus) &
        treatment_60_65plus == "Control (60+ yrs old & No Free Pass)" &
        !is.na(has_cost_prevented_transit)
    ) %>% 
    ggbarstats(
      x = has_cost_prevented_transit,
      y = new_wave,
      digits.perc = 1,
      # package = "ggsci",
      # palette = "category10_d3",
      # arguments relevant for `combine_plots()`
      # title.text = "Passenger survival on the Titanic by gender and age",
      # caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
      plotgrid.args = list(nrow = 2)
      # , ggtheme = theme_ipsum()
      , type = "nonparametric"
      , pairwise.display = "significant"
      # , xlab = ""
      # , ylab = ""
      , title = "Control (60+ yrs old & No Free Pass)"
      # , subtitle = "Average daily trips per wave"
      # , results.subtitle = F
      , legend.title = "Has cost prevented transit use"
      , bf.message = FALSE
      , sample.size.label.args = list(size = 0)
    ) +
    theme_ipsum(
      # legend.title = element_text(family = "roboto", size = 20)
    ) +
    theme(
      legend.title = element_text(family = "roboto", size = 20)
      , text = element_text(family = "roboto", size = 20)
      , plot.title    = element_text(family = "roboto"),
      plot.subtitle = element_text(family = "roboto", size = 12),
      axis.title.x  = element_text(family = "roboto", size = 16),
      axis.title.y  = element_text(family = "roboto", size = 16),
      axis.text.x   = element_text(family = "roboto", size = 16),
      axis.text.y   = element_text(family = "roboto")
      , strip.text.x = element_text(family = "roboto", size = 16)
      , legend.position = "bottom"
      , panel.grid.minor.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.major.x = element_blank()
      , 
    ) +
    ylab("Frequency") +
    xlab("") +
    labs(
      # subtitle = "Average daily trips"
    ) +
    scale_fill_manual(
      # name = "a"
      , values = c("#67a9cf", "#ef8a62")
    )
)

(p_cost_prevented <- p_cost_prevented_control + p_cost_prevented_treat +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
)


# * activity participation --------------------------------------------------



# statistical tests -------------------------------------------------------


