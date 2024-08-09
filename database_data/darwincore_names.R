setwd('/media/steppe/hdd/Barneby_Lives-dev/database_data')

library(tidyverse)

all_names <- read.csv('Fields.csv')

final_names <- filter(all_names, !COLNAMES %in% pop_names$DarwinCore ) %>% 
  mutate(BL = NA) %>% 
  rename(DarwinCore = COLNAMES) %>% 
  bind_rows(., pop_names) %>% 
  slice(match(all_names$COLNAMES, DarwinCore))

write.csv(final_names, 'DarwinCore_names.csv')
