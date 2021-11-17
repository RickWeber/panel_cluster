rm(list = ls())
source("01_libraries.R")
source("02_import_data.R") 
source("03_functions.R"); df <- efw_data_panel


# NOTE TO SELF:
# I want to use gganimate so I can more easily see the transition over time 
# (instead of) just seeing it all at once as below.
df %>% chained_clustering_by_year() %>% draw_map + facet_wrap(~year)
# but it's not behaving. 
p1 <- df %>% chained_clustering_by_year() %>% 
  draw_map 
p2 <- p1 + 
  labs(title = 'Year: {frame_time}') +
  transition_manual(station_install_date, cumulative = TRUE) +
  # transition_time(year) +
  ease_aes('linear')
anim2 <- animate(p2, renderer = gifski_renderer())
p3 <- p1 + 
  labs(title = 'Year: {frame_time}') +
  # transition_manual(station_install_date, cumulative = TRUE) +
  transition_time(year) +
  ease_aes('linear')
anim3 <- animate(p3, renderer = gifski_renderer())
anim2
anim3
# what if the problem is the gaps between years early in the dataset?
p1 <- df %>% filter(year > 1999) %>%
  chained_clustering_by_year() %>%
  draw_map()
p1 + 
  labs(title = 'Year: {frame_time}') +
  # transition_manual(station_install_date, cumulative = TRUE) +
  transition_time(year) +
  ease_aes('linear')
# nope. Every 5th year?
p1 <- df %>% filter(as.integer(lubridate::year(year)) %% 5 == 0) %>%
  chained_clustering_by_year() %>%
  draw_map()
p1 + 
  labs(title = 'Year: {frame_time}') +
  # transition_manual(station_install_date, cumulative = TRUE) +
  transition_time(year) +
  ease_aes('linear')
# also nope.