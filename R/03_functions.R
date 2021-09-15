rm(list = ls())
source("01_libraries.R")
source("02_import_data.R") ; df <- efw_data_panel

### Widen data
efw_widen <- function(df, vars=paste0("efw",1:5)){
  df %>%
    select(-overall) %>%
    pivot_wider(names_from = "year",
                values_from = vars)
}
### Widen then cluster
cluster_wide <- function(df, k = 4){
  out <- cbind((df %>%
                  efw_widen),
               (df %>%
                  efw_widen %>% 
                  select(contains("efw")) %>%
                  scale %>%
                  dist %>%
                  kmeans(4))$cluster) %>%
    as_tibble
  colnames(out)[ncol(out)] <- "cluster"
  out
}

### Add principal components on to a dataframe
add_pca_efw <- function(df,components = 2){
  # add first two principal components onto a data frame
  pca <- df %>%
    select(contains("efw")) %>%
    na.omit %>%
    prcomp
  pca <- pca$x[,paste0("PC",1:components)]
  cbind(na.omit(df),pca) %>% as_tibble
}

### Calculate a distance matrix between each row using Gower distance.
gower <- function(df){
  map(1:nrow(df),function(x){
    gower_dist(df[x,],df)
  }) %>% unlist %>% matrix(.,ncol = nrow(df))
} 

### Year-wise clustering
cluster_by_year_full <- function(df, k = 4){
  df %>%
    group_by(year) %>%
    nest %>%
    mutate(cluster_object = map(data,function(d){
      d %>% select(contains("efw")) %>% na.omit %>% scale %>% kmeans(k)
    })) %>%
    mutate(clustered_data = map2(data,cluster_object,function(d,c){
      cluster = c$cluster
      cbind(d %>% na.omit,cluster) %>% as_tibble
    }))
} 

cluster_by_year <- function(df, k = 4){
  cluster_by_year_full(df, k) %>% 
    select(clustered_data) %>%
    unnest
}

source("03a_alignment.R")

df %>% cluster_by_year(4) %>% cluster_alignment(4)

### Chained k-means

chained_clustering_by_year <- function(df, k = 4){
  yrs <- unique(df$year) %>% sort
  first_df <- df %>%
    filter(year == first(yrs)) %>% 
    na.omit 
  # print(nrow(first_df))
  first_k <- first_df %>% 
    select(contains("efw")) %>%
    scale %>%
    kmeans(k)
  cent <- first_k$centers
  # print(cent)
  out_df <- cbind(first_df,cluster = first_k$cluster) %>% as_tibble()
  for(y in setdiff(yrs,first(yrs))){
    # print(y)
    next_df <- df %>%
      filter(year == y) %>%
      na.omit
    next_k <- next_df %>%
      select(contains("efw")) %>%
      scale %>%
      kmeans(cent)
    cent <- next_k$centers
    # print(cent)
    next_df <- cbind(next_df, cluster = next_k$cluster)
    out_df <- full_join(out_df,next_df)
  }
  return(out_df)
} # ; df %>% chained_clustering_by_year() %>% plot_membership()
# That looks a lot better than the alignment stuff above.
# This is still clustering by year, just starting the algorithm with the results 
# from the year before.
# Question: are the results affected by a widening of the dataset? e.g., if one year has a lot of new countries, 
# could that result in cluster centers shifting (relatively) dramatically?

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

###########################
# test the cluster alignment function

t1 <- df %>% cluster_by_year %>% select(-cluster_object,-data) %>% unnest(clustered_data)
t2 <- t1 %>% cluster_alignment

t3 <- t1 %>% group_by(cluster,year) %>% summarize_if(is.double,mean) %>% ungroup %>%
  add_pca_efw() %>%
  mutate(aligned = FALSE)
# colnames(t3)[8:9] <- c("PC1","PC2")
t3 %>% ggplot(aes(PC1,PC2,color = factor(cluster))) + geom_point() + facet_wrap(~year)

t4 <- t2 %>% group_by(cluster,year) %>% summarize_if(is.double,mean) %>% ungroup %>%
  add_pca_efw() %>% 
  mutate(aligned = TRUE)
# colnames(t4)[8:9] <- c("PC1","PC2")
t4 %>% ggplot(aes(PC1,PC2,color = cluster)) + geom_point() + facet_wrap(~year)

full_join(t3,t4) %>% 
  ggplot(aes(PC1,PC2)) +
  geom_point(aes(color = factor(cluster))) +
  facet_grid (aligned ~ year)

t2 %>% plot_membership

full_join(t3,t4) %>%
  ggplot(aes(year,PC1)) +
  geom_line(aes(color = factor(cluster))) +
  facet_wrap(~aligned)

full_join(t3,t4) %>%
  group_by(aligned,cluster,year) %>%
  summarize_if(is.double,mean)
# Okay, the labels aren't stable in the way I'd hoped for, but looking at that 
# second from last plot I can see that there are some trajectories I want to capture 
# (see image file from Aug 10). The aligned clusters seem to keep their labels,
# but sometimes two clusters swap labels for a while. Like from 2000 to 2004 cluster 2 
# is stable, then switches labels with cluster 3. Cluster 4 gets muddled with cluster 1
# around the same time.
######################

