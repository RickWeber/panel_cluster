rm(list = ls())
source("libraries.R")
source("import_data.R") ; df <- efw_data_panel


# Calculate a distance matrix between each row.
gower <- function(df){
  map(1:nrow(df),function(x){
    gower_dist(df[x,],df)
  }) %>% unlist %>% matrix(.,ncol = nrow(df))
} 

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
  kdata <- df %>% filter(Year == yr) %>% gower %>% kmeans(k)
  df[df$Year == yr,'cluster'] <- kdata$cluster
  return(list(df=df,kdata=kdata))
}
# use another year's clustering outcome to add information to this years
# then cluster. 
cluster2cluster <- function(df,yr,kdata){
  df1 <- df %>% filter(Year == yr) %>% mutate(cluster = kdata$cluster)
  df1 %>% gower %>% kmeans(centers = kdata$centers)
} 
# Cluster the latest year then use the results to cluster each 
# prior year 
cluster_back <- function(df, k){
  yrs <- grab_yrs(df)
  cluster1 <- cluster_yr(df,yrs[1],k)
  kdata <- cluster1$kdata
  df <- cluster1$df
  for(i in 2:length(yrs)){
    y <- yrs[i]
    kdata <- cluster2cluster(df,y,kdata)
    df <- df[Year == y,'cluster'] <- kdata$cluster
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
    df <- df[Year == y,'cluster'] <- kdata$cluster
  }
  return(df)
}


# Standardize column headers that look like 'year', 'country', or 'iso'
# These functions will make the library more general purpose.
yearify_cols <- function(df){
  FALSE
}