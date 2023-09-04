# Geographic extent of tested data

library(sf)
library(tidyverse)
library(tigris)
library(rnaturalearth)

polybb <- st_as_sfc(
  st_bbox(
    c(xmin = -85, 
      xmax = -125, 
      ymax = 30, 
      ymin = 50
      ), 
    crs = st_crs(4326)
    )
  ) 

lakes <- ne_download(category = 'physical', type='lakes', scale = 'small') %>% 
  st_as_sf() %>% 
  filter(name_alt == 'Great Lakes') %>% 
  st_make_valid() 

countries <- ne_countries() %>% 
  st_as_sf() %>% 
  filter(name %in% c('United States of America', 'Canada', 'Mexico'))

states <- states() %>% 
  st_transform(4326) 

test_data <- read.csv('data/test_data.csv') %>% 
  drop_na(longitude) %>% 
  drop_na(latitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

ggplot() +
  geom_sf(data = countries) + 
  geom_sf(data = states) + 
  geom_sf(data = lakes, fill = 'blue') + 
  geom_sf(data = test_data, shape = 1) +
  geom_sf(data = polybb, fill = NA, linewidth = 2, color = '#2F0A28') + 
  coord_sf(xlim = c(-85, -125), ylim = c(30, 50))

