source("libraries.R")
if(!file.exists("../data/EFW_data_raw.xlsx")){
  efw_data_url <- "https://www.fraserinstitute.org/sites/default/files/efw-2020-master-index-data-for-researchers.xlsx"
  download.file(efw_data_url,destfile = "../data/EFW_data_raw.xlsx")
}
efw_data_raw <- read_excel("../data/EFW_data_raw.xlsx",sheet = 1, skip = 4)
efw_data_panel <- read_excel("../data/EFW_data_raw.xlsx",sheet = 2)
efw_data_pre1970s <- read_excel("../data/EFW_data_raw.xlsx",sheet = 3)