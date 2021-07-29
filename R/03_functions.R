rm(list = ls())
source("01_libraries.R")
source("02_import_data.R") ; df <- efw_data_panel


# Calculate a distance matrix between each row.
gower <- function(df){
  map(1:nrow(df),function(x){
    gower_dist(df[x,],df)
  }) %>% unlist %>% matrix(.,ncol = nrow(df))
} 

# update defaults for kmeans
# kmeans <- function(x, centers = 4, iter.max = 20L, nstart = 1L){
#   kmeans(x, centers = centers, iter.max = iter.max, nstart = nstart)
# }

##### June 1 work
# Focus on aligning clusters.
# If I've got two datasets with clusters that aren't already identical, 
# then I want to either have centroids, or find sample centroids, then
# use those centroids to re-align then redefine the clusters.
align_clusters2 <- function(clustered_data1, clustered_data2){
  df1 <- clustered_data1 ; df2 <- clustered_data2
  df3 <- full_join(df1, df2,
                   by = c("Countries", "cluster"),
                   suffix = c(".1",".2"))
  # Already aligned case:
  if(df3$cluster.1 == df3$cluster.2){
    return(full_join(df1,df2))
  }
  # Else, find centroids for df1$cluster
  centroids.1 <- df1 %>% find_centroids
}

find_centroids(df,cluster_var = "cluster"){
  df %>% group_by(cluster_var) %>%
    summarize_if(is.numeric,funs(mean(.,na.rm=TRUE)))
}

df_clustered <- df %>% 
  group_by(Year) %>% 
  nest %>%
  mutate(dist = map(data,gower),
         k = map(dist,function(x){
           kmeans(x,4)
         })) # from here I need to extract cluster membership.

extract_cluster <- function(kdata){
  # pull cluster membership from a nested
}
align_clusters <- function(cluster_var,group_var,first=TRUE){}
##### Work before June 1
# Convenience function for grabbing the years from the dataset.
grab_yrs <- function(df){
  unique(df$Year) %>% sort(decreasing = TRUE)
}
# Get two adjacent years
two_yrs <- function(df,yr,prev = TRUE){
  yrs <- grab_yrs(df)
  yr_i <- match(yr,yrs)
  yr_j <- ifelse(prev,yr_i-1,yr_i+1)
  return(yrs[c(yr_i,yr_j)])
}
# cluster a single year based on underlying data alone.
cluster_yr <- function(df,yr,k){
  kdata <- df %>% filter(Year == yr) %>% gower %>% kmeans(k, nstart = 10)
  df[df$Year == yr,'cluster'] <- kdata$cluster
  return(list(df=df,kdata=kdata))
}
# use another year's clustering outcome to add information to this years
# then cluster. 
cluster2cluster <- function(df,yr,kdata){
  df <- df %>% filter(Year == yr) 
  df[df$Year == yr,'cluster'] <- kdata$cluster
  df %>% gower %>% kmeans(centers = kdata$centers)
} 
# Cluster the latest year then use the results to cluster each 
# prior year 
cluster_back <- function(df, k){
  yrs <- grab_yrs(df)
  cluster1 <- cluster_yr(df,yrs[1],k)
  kdata <- cluster1$kdata
  df <- cluster1$df
  for(i in 2:length(yrs)){ # replace with seq_along?
    y <- yrs[i]
    kdata <- cluster2cluster(df,y,kdata)
    df <- df[df$Year == y,'cluster'] <- kdata$cluster
  }
  return(df)
}
# Same as above, but starting earlier in time and walking forward.
cluster_fore <- function(df, k){
  yrs <- grab_yrs(df) %>% rev
  cluster1 <- cluster_yr(df,yrs[1],k)
  kdata <- cluster1$kdata
  df <- cluster1$df
  for(i in 2:length(yrs)){
    y <- yrs[i]
    kdata <- cluster2cluster(df,y,kdata)
    df <- df[df$Year == y,'cluster'] <- kdata$cluster
  }
  return(df)
}

# debugging 
t <- cluster_yr(df,2018,4)
t1 <- cluster2cluster(df, 2017, t$kdata)
cbind(t1$cluster, t$df$cluster) %>% head(length(t1$cluster))
# looks like it's doing what it's supposed to...
# but I'm getting this error:
#
# Error in UseMethod("filter") : 
#   no applicable method for 'filter' applied to an object of class "c('integer', 'numeric')"
# In addition: There were 50 or more warnings (use warnings() to see the first 50)
#
# when I run: t <- cluster_fore(df,4)

# Standardize column headers that look like 'year', 'country', or 'iso'
# These functions will make the library more general purpose.
yearify_cols <- function(df){
  FALSE
}