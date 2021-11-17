
# map data
map_coords <- map_data("world") %>%
  as_tibble() %>%
  mutate(iso3c = countrycode(region,"country.name","iso3c"))

### mapping function
draw_map <- function(df){
  df %>% 
    right_join(map_coords) %>% 
    ggplot(aes(long,lat,group = group)) + 
    geom_polygon(aes(fill = as.factor(cluster)),color = alpha("white", 1/2), size = 0.2) +
    theme(legend.position = "none") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_blank()) +
    labs(title =  "",
         x = "",
         y = "") + scale_fill_viridis_d()
} 

df %>% cluster_wide %>% draw_map
# df %>% cluster_wide %>% fviz_cluster
