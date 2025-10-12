setwd('~/Documents/assoRted/Barneby_Lives_dev/valleys2')

library(sf)
library(tidyverse)
library(rmapshaper)
library(tigris)

v1 <- st_read(file.path('valleys', 'valleys.shp')) |>
  st_cast('POLYGON') |>
  mutate(TYPE = 'VALLEY') |>
  st_transform(4326)

st <- tigris::states()
st_s <- filter(st, STUSPS %in% c('CA', 'OR', 'WA', 'NV', 'ID', 'MT', 'CO', 'AZ', 'NM', 'WY', 'UT')) |>
  st_combine() |>
  st_make_valid() |>
  st_transform(4326)

v2 <- st_read(file.path('valleys', 'valleys-heartland.shp')) |>
  st_cast('POLYGON') |>
  mutate(TYPE = 'VALLEY') |>
  st_transform(4326)

v2 <- v2[unlist(st_intersects(st_s, v2)),]

rm(st, st_s)
# COMBINE
valleys <- bind_rows(v1, v2) 

# rm(v1, v2)

# NOW READ IN GMBA BEFORE SIMPLIGYING GEOMETRY

mt <- st_read(file.path('mountains', 'mountains.shp')) |>
  mutate(TYPE = 'MOUNTAIN') |>
  select(-Mountains) |>
  st_transform(4326)

data <- bind_rows(v1, mt)

# keep compute low, run the pieces separately
data <- st_make_valid(data)
data_0.5 <- ms_simplify(data, keep = 0.025, keep_shapes = TRUE) 
data_0.5 <- filter(data_0.5, TYPE == 'VALLEY')
data_0.5 <- data_0.5[st_geometry_type(data_0.5) %in%   c("POLYGON", "MULTIPOLYGON"), ]
data_0.5 <- st_make_valid(data_0.5)
data_v1 <- st_cast(data_0.5, 'POLYGON')

data <- bind_rows(v2, mt)

data <- st_make_valid(data)
data_0.5 <- ms_simplify(data, keep = 0.025, keep_shapes = TRUE) 
data_0.5 <- filter(data_0.5, TYPE == 'VALLEY')
data_0.5 <- data_0.5[st_geometry_type(data_0.5) %in%   c("POLYGON", "MULTIPOLYGON"), ]
data_0.5 <- st_make_valid(data_0.5)
data_v2 <- st_cast(data_0.5, 'POLYGON')

valleys <- bind_rows(data_v1, data_v2)

rm(data, data_0.5, data_v1, data_v2, mt, v1, v2)

v3 <- valleys |>
  group_by(HU10_NAME) |>
  reframe(geometry = st_union(geometry)) |>
  st_as_sf() |>
  st_make_valid() 

sizes <- as.numeric(units::set_units(st_area(v3), "km^2"))
table(sizes > 20)

v3 <- v3[sizes > 20,]

v3 <- v3 |>
  mutate(ID = 1:n()) |>
  st_cast('POLYGON')

st_write(v3, 'valleys_master.gpkg', append = F)
