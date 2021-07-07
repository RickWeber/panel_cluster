# Blank slate. I'll reintegrate with functions.R later.
# For now, I'll get ideas out of my brain without the bias of the work I 
# did before. 


### Cluster wrapper
cluster_wrapper <- function(df, distmethod = wide_dist, clustermethod = kmeans , k = 4){
  df %>%
    distmethod %>%
    clustermethod(k)
}
# With that structure, I could map across a bunch of distance and cluster methods
# and have all the data in a single tidy dataframe! 

# Nothing fancy approach:
# Cluster the countries using all year's data at once.
# widen the dataframe to have as many rows as countries and many columns.
wide_dist <- function(df,yearvar='Year',distmethod=gower){
  df %>%
    pivot_wider(names_from = yearvar, values_from = . ) %>%
    distmethod
}
# With weighting by year...
# get the distance for each year, then multiply the distance matrix for each year by that weight
# weighted_by_year_dist <- function(df,yearvar='Year',distmethod=gower,weights=1){
#   df <- df %>%
#     group_by(yearvar) %>%
#     nest %>%
#     mutate(dist = distmethod(data))
#   
#   %>%
#     mutate(dist = )
#     pivot_wider(names_from = yearvar, values_from = . ) %>%
#     distmethod
# }
### I'm not sure I'm wrapping my head around that one correctly. I'll have to 
# think this one out on paper.