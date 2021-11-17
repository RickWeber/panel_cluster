
### Visualize cluster membership over time
plot_membership <- function(clustered_panel_df){
  clustered_panel_df %>%
    ggplot(aes(year,country,fill = as.factor(cluster))) +
    geom_tile() +
    theme_minimal() +
    scale_fill_brewer(type = "qual",
                      palette = "Set1")
}
