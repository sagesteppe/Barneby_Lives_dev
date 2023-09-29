library(tidyverse)
library(BarnebyLives)
library(googlesheets4)

setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript/labels')
dir.create('raw')
dir.create('processed')
dir.create('final')

p <- '/media/steppe/hdd/Barneby_Lives-dev/manuscript/labels/raw'

#p2libs <- .libPaths()[ # find the skeleton here and copy to local directory 
#  grepl(paste0(version$major, '.', sub('\\..*', "", version$minor)), 
#        .libPaths())]

folds <- c('skeleton.Rmd')

data <- readRDS('../data/processed/data_w_Google_Maps') |>
  sf::st_drop_geometry() 
time_label_gen <- system.time({
  purrr::walk(
    .x = collections$Collection_number,
    ~ rmarkdown::render(
      input = folds,
      output_file = file.path(p, glue::glue("{.x}.pdf")),
      params = list(Collection_number = {.x})
    )
  )
})

saveRDS(time_label_gen, file = '../data/processed/time_powo_searcher')