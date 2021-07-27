# Blank slate. I'll reintegrate with functions.R later.
# For now, I'll get ideas out of my brain without the bias of the work I 
# did before. 
rm(list = ls())
source("libraries.R")
source("import_data.R") ; df <- efw_data_panel

# visualize clusters

quick_pca <- function(df, data_cols){
  # take a df with data columns and a cluster column
  # apply PCA to reduce it to 2 dimensions
  # return df with 
  pca <- (df %>% 
            select(all_of(data_cols)) %>%
            princomp)$scores[,1:2] %>%
    as_tibble
  pca <- pca %>%
    rename(pca1 = Comp.1,
           pca2 = Comp.2)
  cbind(df,pca)
} 

pca_plot <- function(df, data_cols){
  df <- quick_pca(df, data_cols)
  df %>%
    ggplot(aes(pca1,pca2))
}

# Quick and dirty test:
# USArrests %>% mutate(k = rep(1:5,10)) %>% pca_plot(colnames(USArrests)) + geom_point(aes(color = factor(k)))
# And with not totally made up clusters:
# t <- cbind((USArrests %>% kmeans(4))$cluster, USArrests)
# t %>% pca_plot(colnames(t[2:5])) + geom_point(aes(color = factor(k)))


### Widen data
efw_widen <- function(df, vars=paste0("efw",1:5)){
  df %>%
    select(-overall) %>%
    pivot_wider(names_from = "year",
                values_from = vars)
}

df_wide <- df %>%
  efw_widen %>%
  cluster_wrapper(distmethod = dist)

### Cluster alignment
## Sample data
# Run year-wise clusters with kmeans (k=4) on the EFW panel.
# Use this data to test functions aligning the cluster membership
## Convenience functions
filter_fcn <- function(df){
  df %>%
    select(-overall)#,-iso3c,-country)
}

scale_if <- function(df){
  df %>%
    mutate_if(is.numeric,scale)
}# visualize clusters

quick_pca <- function(df, data_cols){
  # take a df with data columns and a cluster column
  # apply PCA to reduce it to 2 dimensions
  # return df with 
  pca <- (df %>% 
            select(all_of(data_cols)) %>%
            princomp)$scores[,1:2] %>%
    as_tibble
  pca <- pca %>%
    rename(pca1 = Comp.1,
           pca2 = Comp.2)
  cbind(df,pca)
} 

pca_plot <- function(df, data_cols){
  df <- quick_pca(df, data_cols)
  df %>%
    ggplot(aes(pca1,pca2))
}
# Quick and dirty test:
# USArrests %>% mutate(k = rep(1:5,10)) %>% pca_plot(colnames(USArrests)) + geom_point(aes(color = factor(k)))
# And with not totally made up clusters:
# t <- cbind((USArrests %>% kmeans(4))$cluster, USArrests)
# t %>% pca_plot(colnames(t[2:5])) + geom_point(aes(color = factor(k)))


dist_if <- function(df){
  df %>% select_if(is.numeric) %>% dist
}

df_sample <- df %>% 
  group_by(year) %>%
  nest


t <- df_sample %>%
  mutate(data1 = map(data,filter_fcn)) %>%
  mutate(data2 = map(data1,scale_if)) %>%
  mutate(dist = map(data2,dist_if)) %>%
  mutate(k = map(data2,function(x){
    df <- x %>%
      filter(complete.cases(.))
    countries <- df$country
    k <- df %>% ungroup %>%
       select_if(is.numeric) %>% 
      dist %>%
       kmeans(.,centers = 4)
     cbind(df,k)
    }))


  mutate(scaled_data = mutate_if(data,is.numeric,scale))
  mutate(k = kmeans(data,4))


df %>%
  group_by(year) %>%
  select(contains("efw")) %>%
  nest() %>%
  mutate(dist = gower(data))
