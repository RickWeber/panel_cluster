rm(list = ls())
source("import_data.R")
# Here's an idea: recursive clustering.
# Take the latest (most complete) year of the panel, cluster that, then add 
# cluster membership to the dataframe of the prior year and cluster on that.
# for mixed discrete and continuous variables I'll need something besides
# Manhattan or Euclidean distance though...
# The Internet suggests Gower's distance
# https://medium.com/analytics-vidhya/gowers-distance-899f9c4bd553
# @article{gower1971general,
#   title={A general coefficient of similarity and some of its properties},
#   author={Gower, John C},
#   journal={Biometrics},
#   pages={857--871},
#   year={1971},
#   publisher={JSTOR}
# }
# see also: 
# https://cran.r-project.org/web/packages/gower/vignettes/intro.pdf
