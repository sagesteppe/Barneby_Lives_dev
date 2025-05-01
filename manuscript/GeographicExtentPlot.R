# Geographic extent of tested data

library(sf)
library(tidyverse)
library(tigris)
library(rnaturalearth)
#setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript')
setwd('/home/sagesteppe/Documents/assoRted/Barneby_Lives_dev/manuscript')

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
  filter(name %in% c('United States of America', 'Canada', 'Mexico')) %>% 
  mutate(Cntry_grp = if_else(sov_a3 %in% c('MEX', 'CAN'), 'International', 'Domestic'))

states <- states() %>% 
  st_transform(4326) 

test_data <- read.csv('./data/test_data.csv') %>% 
  drop_na(Longitude) %>% 
  drop_na(Latitude) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

sites <- distinct(test_data, geometry)

cols <- c("International" = "gray55", "Domestic" = "#e6f4f1")


png('../graphics/plots/collections_map.png')
ggplot() +
  geom_sf(data = countries, aes(fill = Cntry_grp)) + 
  scale_fill_manual(values = cols, guide = 'none') +
  geom_sf(data = states, fill = NA, color = 'grey5') + 
  geom_sf(data = lakes, fill = '#176B87') + 
  
  geom_sf(data = sites, shape = 21, color = 'gold', fill = '#2F0A28', size = 2) +
  
  
  geom_sf(data = polybb, fill = NA, linewidth = 2, color = '#94524A') + 
  coord_sf(xlim = c(-85, -125), ylim = c(30, 50)) +
  theme(
    panel.background = element_rect(fill = "#176B87", colour = "#176B87"),
    panel.grid.major = element_line(colour = "#176B87"),
    plot.title = element_text(hjust = 0.5)) +
  
  labs(title = 'Spatial Extent of Example BarnebyLives Geodata and Collections')

dev.off()
