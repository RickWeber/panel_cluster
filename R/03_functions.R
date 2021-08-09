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
    # choose a mapping to minimize total distance between centers
    mapping <- choose_mapping(distances)
    # Let's minimize the sum of squared differences
    # I can represent the mapping with a permutation of 1:nclusters
    # that maps each of the new clusters to the original cluster labels
    # I want to choose the mapping that minimizes the distance between 
    # centers
    mapping <- sample(1:nclusters) # random mapping for testing
    df1 <- df1 %>%
      relabel(mapping)
    # Now I can overwrite the data I started with
    out[out$year == y,"cluster"] <- df1$cluster
  }
  out
}

###########################
# test

t1 <- df %>% cluster_by_year %>% select(-cluster_object,-data) %>% unnest()
t2 <- t1 %>% cluster_alignment

t2 %>%
  ggplot(aes(year,country)) +
  geom_tile(aes(fill = as.factor(cluster))) + 
  theme_minimal() +
  scale_fill_brewer(type = "qual",
                    palette = "Set1")
# I'm not convinced this thing is doing what I want it to.
# It's definitely changing some labels, but the plot is still showing wild change
# in labels over time.

######################

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

# map data
map_coords <- map_data("world") %>%
  as_tibble() %>%
  mutate(iso3c = countrycode(region,"country.name","iso3c"))

df %>% 
  cluster_wide %>% 
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
