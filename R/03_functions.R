# remove below
rm(list = ls())
source("01_libraries.R")
source("02_import_data.R") ; df <- efw_data_panel
# remove above. Just there for debugging

### Import function files
source("03a_clustering_functions.R")
source("03b_data_manipulation_functions.R")
source("03c_visualization_functions.R")
source("03d_animation_functions.R")


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




########

df %>% cluster_by_year(4) %>% cluster_alignment(4)

 # ; df %>% chained_clustering_by_year() %>% plot_membership()
# That looks a lot better than the alignment stuff above.
# This is still clustering by year, just starting the algorithm with the results 
# from the year before.
# Question: are the results affected by a widening of the dataset? e.g., if one year has a lot of new countries, 
# could that result in cluster centers shifting (relatively) dramatically?


###########################
# test the cluster alignment function

t1 <- df %>% cluster_by_year %>% select(-cluster_object,-data) %>% unnest(clustered_data)
t2 <- t1 %>% cluster_alignment

t3 <- t1 %>% group_by(cluster,year) %>% summarize_if(is.double,mean) %>% ungroup %>%
  add_pca_efw() %>%
  mutate(aligned = FALSE)
# colnames(t3)[8:9] <- c("PC1","PC2")
t3 %>% ggplot(aes(PC1,PC2,color = factor(cluster))) + geom_point() + facet_wrap(~year)

t4 <- t2 %>% group_by(cluster,year) %>% summarize_if(is.double,mean) %>% ungroup %>%
  add_pca_efw() %>% 
  mutate(aligned = TRUE)
# colnames(t4)[8:9] <- c("PC1","PC2")
t4 %>% ggplot(aes(PC1,PC2,color = cluster)) + geom_point() + facet_wrap(~year)

full_join(t3,t4) %>% 
  ggplot(aes(PC1,PC2)) +
  geom_point(aes(color = factor(cluster))) +
  facet_grid (aligned ~ year)

t2 %>% plot_membership

full_join(t3,t4) %>%
  ggplot(aes(year,PC1)) +
  geom_line(aes(color = factor(cluster))) +
  facet_wrap(~aligned)

full_join(t3,t4) %>%
  group_by(aligned,cluster,year) %>%
  summarize_if(is.double,mean)
# Okay, the labels aren't stable in the way I'd hoped for, but looking at that 
# second from last plot I can see that there are some trajectories I want to capture 
# (see image file from Aug 10). The aligned clusters seem to keep their labels,
# but sometimes two clusters swap labels for a while. Like from 2000 to 2004 cluster 2 
# is stable, then switches labels with cluster 3. Cluster 4 gets muddled with cluster 1
# around the same time.
######################

