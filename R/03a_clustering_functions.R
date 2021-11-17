
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
}

## realign cluster labels
### find cluster centers
cluster_centers <- function(clustered_df){
  # Convenience function
  clustered_df %>% ungroup %>%
    group_by(cluster) %>%
    # should explicitly select data columns
    # or maybe I just select out columns I don't want before
    # running data through this function
    summarize_if(is.double,funs(mean(.,na.rm = T))) %>%
    ungroup %>%
    arrange(cluster) %>%
    select(-cluster)
}

### measure distances between cluster centers
center_distances <- function(base_centers, next_centers){
  # Note:
  # element [i,j] of the output shows the distance between
  # base_center[i,] and next_center[j,]
  base_k <- nrow(base_centers)
  next_k <- nrow(next_centers)
  if (base_k != next_k) return("ERROR")
  k <- base_k
  map(1:k,
      function(i){
        map_dbl(1:k,
                function(j){
                  # get distance between center j from next and center i from base
                  rbind(base_centers[i,],
                        next_centers[j,]) %>%
                    dist
                  # put it into element i,j of distances
                })
      }) %>% unlist %>% 
    matrix(nrow = k) %>% t 
}

### sum of square distances
evaluate_mapping <- function(mapping, distances){
  diag(distances[mapping,])^2 %>% sum
}

### choose a mapping to minimize distances
choose_mapping <- function(distances){
  possible_mappings <- permutations(ncol(distances),ncol(distances))
  nmappings <- nrow(possible_mappings)
  scores <- map_dbl(1:nmappings,
                    function(m){
                      evaluate_mapping(possible_mappings[m,],distances)
                    })
  possible_mappings[which.min(scores),]
}

### relabel clusters based on a given mapping
relabel <- function(df, mapping){
  k <- length(mapping)
  df <- df %>%
    mutate(cluster = cluster + k)
  for (c in 1:k) {
    df <- df %>%
      mutate(cluster = ifelse(cluster == (c + k),
                              mapping[c],
                              cluster))
  }
  df
}

