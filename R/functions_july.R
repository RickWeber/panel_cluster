# Blank slate. I'll reintegrate with functions.R later.
# For now, I'll get ideas out of my brain without the bias of the work I 
# did before. 
rm(list = ls())
source("01_libraries.R")
source("02_import_data.R")
# rename the data of interest because I'm lazy...
df <- efw_data_panel

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

add_pca <- function(df,components = 2){
  # add first two principal components onto a data frame
  pca <- df %>%
    select(contains("efw")) %>%
    na.omit %>%
    prcomp
  pca <- pca$x[,paste0("PC",1:components)]
  cbind(na.omit(df),pca)
}

### test
# cluster and visualize
cluster_wide(df,4) %>% 
  add_pca %>%
  ggplot(aes(PC1,PC2,color = factor(cluster))) + geom_point()
# That leaves only 50 countries that have a complete dataset. But if I lop off 
# the earliest years, I should be able to get a bigger group.
df %>%
  filter(year > 1970) %>%
  cluster_wide %>%
  add_pca %>%
  ggplot(aes(PC1,PC2,color = factor(cluster))) + geom_point()
# Sure enough! We'll have to compare a few of these sets at some point.

############
# year-wise clustering
cluster_one_year <- function(df, k = 4, include_stats=FALSE){
  # kmeans clustering for a single year
  # returns original data plus cluster membership
  df <- df %>% 
    # na.omit %>% # is na.omit necessary?
    as_tibble # This helps purrr::map
  out <- df %>%
  cluster_object <- df %>%
    select_if(is.double) %>%
    scale() %>%
    kmeans(.,k)
  cluster <- cluster_object$cluster
  out <- cbind(out,cluster) %>% as_tibble
  if(include_stats){
    return(list(df = out,stats = cluster_object))
  }
  out
}

cluster_by_year <- function(df, k = 4){
  df %>% 
    # na.omit %>% # is na.omit necessary?
    select(-overall) %>%
    group_by(year) %>%
    nest %>%
    mutate(cluster = map(data,cluster_one_year(k)))
}


# df %>% filter(year > 1990) %>% cluster_by_year

# Cluster alignment
# I'm finally getting dplyr and purrr to behave and now kmeans is giving me problems. 
# so I'm going to get super lazy and just make some random clusters to create 
# sample data so I can write the alignment cod
#
clustered_panel_df <- df %>%
  mutate(cluster = sample(1:4,nrow(df),TRUE)) 

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

# The plan:
# take a df, group by year, and make up new cluster labels
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

## Test
# aligned_df <- clustered_panel_df %>% cluster_alignment
# aligned_df$cluster - clustered_panel_df$cluster %>% hist()
# It looks like it's reaarranging labels, but I need to get decent data before I 
# trust it

####################
### Graveyard ######
####################
# There we go! The function code needs work, but I can at least get the plot. 
# This also gives me an idea about aligning the clusters...
# I could put two years through PCA, together and match clusters by nearest neighbors.
# ### Cluster alignment
# ## Sample data
# # Run year-wise clusters with kmeans (k=4) on the EFW panel.
# # Use this data to test functions aligning the cluster membership
# 
# scale_if <- function(df){
#   df %>% 
#     select_if(is.numeric) %>%
#     scale
# }
# 
# cluster_fcn <- function(df,k=4){
#   clust <- df %>% 
#     as_tibble %>%
#     select(contains("efw")) %>%
#     scale %>%
#     kmeans(k)
#   df %>%
#     mutate(k = clust$cluster)
# }
# 
# 
# t <- df %>%
#   drop_na %>%
#   group_by(year) %>%
# 
#    
# t %>%
#   map_df(data, cluster_fcn)
#   mutate(clustered_data = cluster_fcn(data))
# 
#   
# 
# 
#   filter(year==2010) %>%
#   unnest()
#   mutate(data = scale(data))
#   mutate(k = data %>%
#            select(contains("efw")) %>% 
#            kmeans(4))
# 
# 
# 
# ## Convenience functions
# filter_fcn <- function(df){
#   df %>%
#     select(-overall)#,-iso3c,-country)
# }
# scale_if <- function(df){
#   df %>%
#     mutate_if(is.numeric,scale)
# }
# 
# dist_if <- function(df){
#   df %>% select_if(is.numeric) %>% dist
# }
# 
# df_sample <- df %>% 
#   group_by(year) %>%
#   nest
# 
# 
# t <- df_sample %>%
#   mutate(data1 = map(data,filter_fcn)) %>%
#   mutate(data2 = map(data1,scale_if)) %>%
#   mutate(dist = map(data2,dist_if)) %>%
#   mutate(k = map(data2,function(x){
#     df <- x %>%
#       filter(complete.cases(.))
#     countries <- df$country
#     k <- df %>% ungroup %>%
#        select_if(is.numeric) %>% 
#       dist %>%
#        kmeans(.,centers = 4)
#      cbind(df,k)
#     }))
# 
# 
#   mutate(scaled_data = mutate_if(data,is.numeric,scale))
#   mutate(k = kmeans(data,4))
# 
# 
# df %>%
#   group_by(year) %>%
#   select(contains("efw")) %>%
#   nest() %>%
#   mutate(dist = gower(data))