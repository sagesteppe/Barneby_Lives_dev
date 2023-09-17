setwd('/media/steppe/hdd/Barneby_Lives-dev/scripts')

library(sf)
library(terra)

p <- '../geodata/elevation'
el <- mosaic(sprc(file.path( p, list.files(p))))

pts <- data.frame(
  Col  = c(25, 26),
  x = c(-117.7172, -117.57132), 
  y = c(39.26367, 38.08626)
) |>
  st_as_sf(coords = c('x', 'y'), crs = 4326) |>
  vect()

pts_el <- extract(el, pts)
colnames(pts_el) <- c('site', 'm')
pts_el$ft <- pts_el$m * 3.28084

pts_el
