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
cluster_by_year <- function(df, k = 4){
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
} # ; df %>% cluster_by_year

### Cluster alignment

# Cluster alignment

cluster_centers <- function(df){
  # Convenience function
  df %>% 
    group_by(cluster) %>%
    summarize_if(is.double,funs(mean(.,na.rm = T)))
}

cluster_center_distances <- function(df0,df1){
  nclusters = unique(df0$cluster) %>% length
  centers0 <- df0 %>%
    select(cluster,contains("efw")) %>%
    cluster_centers
  centers1 <- df1 %>%
    select(cluster,contains("efw")) %>%
    cluster_centers
  out <- map(1:nclusters,
             function(x){
               c0 <- centers0 %>%
                 filter(cluster == x) %>%
                 select(-cluster)
               map_dbl((1:nclusters) + nclusters,
                       function(x){
                         c1 <- centers1 %>%
                           filter(cluster == x) %>%
                           select(-cluster)
                         (c0 - c1)^2 %>% 
                           sum %>%
                           sqrt
                       })
             }) %>% 
    unlist %>%
    matrix(ncol = nclusters)
  rownames(out) <- (1:nclusters) + 4
  colnames(out) <- 1:nclusters
  out
}

# Will this distance measure make the alignment behave better?
# Instead of looking for centroids centered in the original dimensions
# of the data, look in the first two principal components.
cluster_principal_distances <- function(df0,df1){
  nclusters = unique(df0$cluster) %>% length
  df0 <- df0 %>%
    add_pca_efw()
  df1 <- df1 %>%
    add_pca_efw()
  centers0 <- df0 %>%
    group_by(cluster) %>%
    summarize_at(contains("PC"),mean) 
  centers1 <- df1 %>%
    group_by(cluster) %>%
    summarize_at(contains("PC"),mean)
  out <- map(1:nclusters,
             function(x){
               c0 <- centers0 %>%
                 filter(cluster == x) %>%
                 select(-cluster)
               map_dbl((1:nclusters) + nclusters,
                       function(x){
                         c1 <- centers1 %>%
                           filter(cluster == x) %>%
                           select(-cluster)
                         (c0 - c1)^2 %>% 
                           sum %>%
                           sqrt
                       })
             }) %>%
    unlist %>%
    matrix(ncol = nclusters)
}

evaluate_mapping <- function(mapping, distances){
  # rearrange the distances matrix based on the mapping
  diag(distances[mapping,])^2 %>% 
    sum
}

choose_mapping <- function(distances){
  possible_mappings <- permutations(ncol(distances),ncol(distances))
  nmappings <- nrow(possible_mappings)
  out <- possible_mappings[1,]
  best_score <- evaluate_mapping(out, distances)
  for (m in 1:nmappings){
    current_score <- evaluate_mapping(possible_mappings[m,],distances)
    if (current_score < best_score){
      out <- possible_mappings[m,]
      best_score <- current_score
    }
  }
  out
}

relabel <- function(df,mapping){
  # relabel a data frame with shifted labels according to a provided mapping
  nclusters = unique(df$cluster) %>% length
  for(c in 1:nclusters){
    df <- df %>%
      mutate(cluster = ifelse(cluster == (c + nclusters),
                              mapping[c],
                              cluster))
  }
  df
}

cluster_alignment <- function(clustered_panel_df){
  out <- clustered_panel_df
  yrs <- unique(out$year) %>% sort
  clusters <- unique(out$cluster) %>% sort
  nclusters <- length(clusters)
  for(y in setdiff(yrs,first(yrs))){
    previous_yr <- yrs[yrs < y] %>% last
    # line up y's cluster labels with the previous year's
    df0 <- out %>% filter(year == previous_yr)
    df1 <- out %>% filter(year == y) %>%
      mutate(cluster = cluster + nclusters) # avoid over-writing
    # Calculate distances between cluster centers
    distances <- cluster_center_distances(df0,df1)
    # distances <- cluster_principal_distances(df0,df1)
    # choose a mapping to minimize total distance between centers
    mapping <- choose_mapping(distances)
    df1 <- df1 %>%
      relabel(mapping)
    # Now I can overwrite the data I started with
    out[out$year == y,"cluster"] <- df1$cluster
  }
  out
}


### Plot cluster membership over time

plot_membership <- function(clustered_panel_df){
  clustered_panel_df %>%
    ggplot(aes(year,country,fill = as.factor(cluster))) +
    geom_tile() +
    theme_minimal() +
    scale_fill_brewer(type = "qual",
                      palette = "Set1")
}

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
} #; df %>% chained_clustering_by_year() %>% plot_membership()
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

