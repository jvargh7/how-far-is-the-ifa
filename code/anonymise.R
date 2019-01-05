path_folder <- "~/Papers/0_How far is the IFA/"


####Anonymise sirohi_village
library(tidyverse)
library(readxl)

sirohi_village <- read_excel(path=paste0(path_folder,"/data/rapid_mcn/EXCEL/Total vil.xlsx"),
                             sheet="Sheet1") %>%  
  filter(DISTRICT ==2) %>% 
  select(VILLAGE_UID,Q3175A1:Q3175B11) %>% 
  mutate(VILLAGE_UID = ifelse(substr(as.character(VILLAGE_UID),5,5)=="2",
                              substr(as.character(VILLAGE_UID),1,7),
                              as.character(VILLAGE_UID)))  %>% 
  
  write_csv(paste0(path_folder,"/data/data_access/final/sirohi_village_public.csv"))


sirohi_pregnant_variables <- read_excel(path=paste0(path_folder,"/data/data_access/final/variable_list_public.xlsx"),
                                        sheet="sirohi_pregnant")

####Anonymise sirohi_pregnant
load("data/rapid_mcn/RAPID_PW_Cleanup.RData")
sirohi_pregnant <- pregnant %>% 
  filter(DISTRICT==2) %>% 
  select(as.character(sirohi_pregnant_variables$name)) %>% 
  write_csv(paste0(path_folder,"/data/data_access/final/sirohi_pregnant_public.csv"))
