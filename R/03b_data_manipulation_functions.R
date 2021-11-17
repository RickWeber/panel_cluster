
### Widen data
efw_widen <- function(df, vars=paste0("efw",1:5)){
  df %>%
    select(-overall) %>%
    pivot_wider(names_from = "year",
                values_from = vars)
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
# Note to self: replace this with https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_cluster
# and related 


### Cluster alignment
# take a set of cluster labels for one year,
# find the centers of each cluster
# take another year's worth of data
# and adjust the labels for clusters
# so that the adjusted labels minimize the distance between
#     centers for the first year and centers for the second year.

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

evaluate_mapping <- function(mapping, distances){
  diag(distances[mapping,])^2 %>% sum
}

choose_mapping <- function(distances){
  possible_mappings <- permutations(ncol(distances),ncol(distances))
  nmappings <- nrow(possible_mappings)
  scores <- map_dbl(1:nmappings,
                    function(m){
                      evaluate_mapping(possible_mappings[m,],distances)
                    })
  possible_mappings[which.min(scores),]
}

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

cluster_alignment_2yrs <- function(df_base, df_next){
  base_centers <- df_base %>% cluster_centers
  next_centers <- df_next %>% cluster_centers
  d <- center_distances(base_centers, next_centers)
  mapping <- choose_mapping(d)
  df_next %>% relabel(mapping)
}

chained_cluster_alignment <- function(clustered_panel_df){
  yrs <- unique(clustered_panel_df$year) %>% sort
  out <- clustered_panel_df %>% filter(year == first(yrs))
  for (y in setdiff(yrs,first(yrs))){
    previous_yr <- yrs[yrs < y] %>% last
    df0 <- clustered_panel_df %>% 
      filter(year == previous_yr)
    df1 <- clustered_panel_df %>% 
      filter(year == y) %>%
      cluster_alignment_2yrs(df0)
    out <- rbind(out,df1)
  }
  out
}

t1 <- df %>% cluster_by_year() %>% ungroup
t2 <- t1 %>% chained_cluster_alignment
# Well that didn't do much...
