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
  df$cluster <- integer(nrow(df))
  df <- cluster_yr(df,yrs[1],k)
  for(i in 2:length(yrs)){
    y <- yrs[i]
    df <- cluster_yr_up(df,y,k)
  }
  return(df)
} ; cluster_back(df,4)

cluster_yr <- function(df,yr,k){
  df <- df %>%
    filter(Year == yr)
  df[df$Year == yr,'cluster'] <- (df %>% gower %>% kmeans(k))$cluster
  return(df)
}

cluster_yr_up <- function(df,yr,k){
  df1 <- df %>% filter(Year == yr)
  # previous year's data should have cluster membership
  yr2 <- two_yrs(df,yr,F) %>% setdiff(yr)
  df2 <- df %>% filter(Year == yr2)
  df3 <- df2 %>% select(-contains("Area"),-Year,-contains("Summary")) %>% full_join(df1)
  df[df$Year == yr,'cluster'] <- (df3 %>% gower %>% kmeans(k))$cluster
  return(df)
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