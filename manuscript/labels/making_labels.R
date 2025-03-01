library(tidyverse)
library(BarnebyLives)

setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript/labels')
dir.create('raw')
dir.create('processed')
dir.create('final')

collections <- read.csv('../data/processed/cleaned_data.csv') %>% 
  mutate(UNIQUEID = paste0(Primary_Collector, Collection_number), 
         Coordinate_Uncertainty = '+/- 5m') %>% 
  data.frame() 

p <- '/media/steppe/hdd/Barneby_Lives-dev/manuscript/labels/raw'

time_label_gen <- system.time({
  purrr::walk(
    .x = collections$Collection_number,
    ~ rmarkdown::render(
      input = 'skeleton.Rmd',
      output_file = file.path(p, glue::glue("{.x}.pdf")),
      params = list(Collection_number = {.x})
    )
  )
})

saveRDS(time_label_gen, file = '../data/processed/time_label_gen')
