# .libPaths('C:/r_packages')
library(tidyverse)

nuclei_data <- read.csv(paste(
  'Relative-Intensity-Quantification/Raw_Data/',
  
  ###############
  'Nuclei_Nuclei',
  ###############
  
  '.csv',
  sep = ''),
  head = TRUE, 
  sep=",")

nuclei_data <- nuclei_data %>% 
  select(c(ObjectNumber, Metadata_Condition, 
           Metadata_Picture, Metadata_Target, 
           Intensity_MeanIntensity_DNA))

target_data <- read.csv(paste(
  'Relative-Intensity-Quantification/Raw_Data/',
  
  ###############
  'Target_Cytoplasm',
  ###############
  
  '.csv',
  sep = ''),
  head = TRUE, 
  sep=",")

target_data <- target_data %>% 
  select(c(ObjectNumber, Metadata_Condition, 
           Metadata_Picture, Metadata_Target, 
           Intensity_MeanIntensity_Target))

