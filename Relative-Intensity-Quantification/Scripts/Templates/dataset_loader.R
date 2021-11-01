# .libPaths('C:/r_packages')
library(tidyverse)
coulumn_selector <- c('ObjectNumber', 'Metadata_Condition', 
                      'Metadata_Picture', 'Metadata_Target', 
                      'Intensity_MeanIntensity_DNA')

nuclei_data <- read.csv(paste(
  'Relative-Intensity-Quantification/Raw_Data/',
  
  ###############
  'Nuclei_Nuclei',
  ###############
  
  '.csv',
  sep = ''),
  head = TRUE, 
  sep=",") %>%

  select(all_of(coulumn_selector))

target_data <- read.csv(paste(
  'Relative-Intensity-Quantification/Raw_Data/',
  
  ###############
  'Target_Cytoplasm',
  ###############
  
  '.csv',
  sep = ''),
  head = TRUE, 
  sep=",") %>%

  select(all_of(coulumn_selector))

