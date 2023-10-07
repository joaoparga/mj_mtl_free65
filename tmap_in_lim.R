
# get interevals
# tm_shape(montreal_cd_ct) +
#   tm_fill(
#     col = "prop_survey"
#     , style = "jenks"
#     # , pal=c("grey",RColorBrewer::brewer.pal(4, "YlOrBr"))
#     )
#
# montreal_cd_ct <- montreal_cd_ct %>%
#   mutate(
#     intervals = cut(
#       prop_survey
#       , breaks = c(0, 0.006, 0.018, 0.036, 0.059, 0.101))
#     )

# tm_shape(montreal_cd_ct) +
#   tm_fill(col = "intervals", palette = "YlGnBu")



tm_shape(montreal_cd_ct) + 
  tm_polygons(
    col = "prop_in_lim_tot"
     # , breaks = c(0,0.0002, 0.0003, 0.0004, 0.0007, 0.0014, Inf)
     , style = "jenks"
    , palette = "Greens"
    , border.alpha = 0.5
    , alpha = 0.95
    # , labels = c("0.00-0.50: Survey underrepresents","0.50-0.99", '0.99-1.01', '1.01-2.00', '2.00 or more: Survey overrepresents')
    , title = "Proportion of individuals in LIM"
  ) + 
  tm_shape(survey_filter_cd) +
  tm_dots(
    col = "in_lim"
    , palette = "RdBu"
    , size = 0.05
    , title = "In LIM (Survey)"
  ) +
  tm_add_legend(
    type = "text"
  ) +
  tm_layout(
    legend.width = 2
    # , main.title = "Ratio of the proportions of survey respondents vs. the population over 50 in each CT"
    # , main.title.position = "center"
    # , main.title.size = 1.05
    , legend.title.size = 1.2
    , legend.text.size = 0.8
  )
