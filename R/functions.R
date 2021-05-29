rm(list = ls())
source("libraries.R")
source("import_data.R") ; df <- efw_data_panel


# Calculate a distance matrix between each row.
gower <- function(df){
  map(1:nrow(df),function(x){
    gower_dist(df[x,],df)
  }) %>% unlist %>% matrix(.,ncol = nrow(df))
} 

# Let's take each year, assign cluster memberships, then use those values to 
# cluster the next year. That is, cluster assignment is a function of this 
# this year's values and next year's cluster membership.



cluster_back <- function(df, k){
  yrs <- grab_yrs(df)
  first_yr <- max(yrs) # first year to cluster on, not first in the data
  last_yr <- min(yrs)
  df$cluster <- NA
  for(i in 1:length(yrs)){
    y <- yrs[i]
    y1 <- yrs[i+1]
    x0 <- df[df$Year == y]
    x1 <- df[df$Year == y1]
    if(y == last_yr){
      return(df)
    }
    if(y == first_yr){
      df[df$Year == y,'cluster'] <- gower(x0) %>% kmeans(k)$cluster
    }
    else{
      xt <- x0 
      xt$cluster <- x1$cluster # I think that'll work. Might be wonky with NAs.
      df[df$Year == y,'cluster'] <- gower(xt) %>% kmeans(k)$cluster
    }
  }
}
cluster_back(df,4)

cluster_yr <- function(df,yr,k){
  clusters <- df %>%  
    filter(Year == yr) %>%
    gower %>%
    kmeans(k)$cluster
  df[df$Year == yr,'cluster'] <- clusters
  return(df)
}

cluster_yr_back <- function(df,yr,k){
  
  yr2 <- yrs[2]
  df1 <- df[df$Year == yr,]
  df2 <- df[df$Year == yr2]
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

# Standardize column headers that look like 'year', 'country', or 'iso'
yearify_cols <- function(df){
  FALSE
}