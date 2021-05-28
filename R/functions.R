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
  yrs <- unique(df$Year) %>% sort(decreasing = TRUE)
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
# cluster backwards through time
cluster_back <- function(df, k){
  yrs <- unique(df$Year) %>% sort(decreasing = TRUE)
  df$cluster <- NA
  for(i in 1:length(yrs)){
    y <- yrs[i]
    y1 <- yrs[i + 1]
    x <- df %>% filter(Year == y)
    d <- gower(x)
    c <- kmeans(d,k)$cluster
    df[df$Year == y1,'cluster'] <- c
  }
} ; cluster_back(df,4)
