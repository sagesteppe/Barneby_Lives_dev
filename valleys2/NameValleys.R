setwd('~/Documents/assoRted/Barneby_Lives_dev/valleys2')

library(sf)
library(spdep)
library(tidyverse)
set.seed(999)

valleys <- st_read('valleys.gpkg') |>
  st_cast('POLYGON') |>
  st_transform(5070) |>
  mutate(ID = 1:n())

sizes <- as.numeric(units::set_units(st_area(valleys), "km^2"))
table(sizes > 20)

large_valleys <- valleys[sizes > 20,]
st_write(large_valleys, 'large_valleys.gpkg', append = FALSE)


bbv <- st_bbox(valleys)

GeoNames_names <- st_read('valleys_glimpse.gpkg') |>
  st_transform(5070) |>
  st_crop(bbv)

total_voron <- st_voronoi( st_union(GeoNames_names))
total_voron <- st_collection_extract(total_voron, "POLYGON") |>
  st_as_sf() |>
  rename(geometry = x) |>
  st_crop(bbv)

total_voron_attributed <- total_voron |>
  st_join(GeoNames_names)

rm(total_voron, bbv)

ggplot() +
  geom_sf(data = total_voron_attributed, fill = NA) +
  geom_sf(data = valleys, col = 'red')


################################################################################

# match one which HU10 watershed has a valley fall in it. 


## identify which watersheds have valleys assigned for them 
ints <- st_intersects(valleys, GeoNames_names)
matched_single <- st_drop_geometry(valleys)[ lengths(ints) == 1, 'ID']

valleys_single_match <- valleys |>
  filter(ID %in% matched_single) |>
  st_join(GeoNames_names, join =  st_contains)

## identify the watersheds with > 1 valley name
valleys_sub <- valleys[ lengths(ints) >= 2, ]



#################################################################################

## using tagged data

twelve_geometry <- sf::st_read(file.path('.', 'data', 'Simplified_huc12_polygons.gpkg')) |>
  st_make_valid()
twelve_geometry <- filter(twelve_geometry, st_is_valid(twelve_geometry))

tagged <- bind_rows(
  sf::st_read(file.path('.', 'valleys',  'Valleys_oct11_2025.gpkg'), quiet = T)|>
    filter(!is.na(Valley)),
  sf::st_read(file.path('.', 'valleys', 'valleys-heartland.shp'), quiet = T) |>
    filter(!is.na(Valley)) |>
    rename(geom = geometry)
) |>
  st_cast('POLYGON') |>
  st_make_valid()

# now simplify the geometries to make overlapping polygons united
tagged <- tagged |>
  group_by(Valley) |>
  summarize(geometry = st_union(geom)) |>
  st_make_valid() |>
  st_cast('POLYGON') |>
  st_make_valid()

# these are fine
# st_write(tagged, file.path('data', 'tagged.gpkg'), append  = F)

overlaps <- st_intersects(twelve_geometry, tagged)

all(st_is_valid(tagged))
all(st_is_valid(twelve_geometry))

twelve_geometry[ lengths(overlaps) > 1, ]

# Step 2: For each high_res feature, find the tagged feature with max overlap
best_tag_idx <- sapply(1:nrow(twelve_geometry), function(i) {
  
  # Get indices of overlapping tagged features
  tag_indices <- overlaps[[i]]
  
  # No overlaps? Return NA
  if (length(tag_indices) == 0) return(NA_integer_)
  
  # One overlap? Easy money
  if (length(tag_indices) == 1) return(tag_indices)
  
  # Multiple overlaps - compute intersection areas for ALL candidates
  overlap_areas <- sapply(tag_indices, function(j) {
    tryCatch({
      intersection <- st_intersection(twelve_geometry[i, ], tagged[j, ])
      # Sum areas in case we get multiple geometries back
      sum(st_area(intersection))
    }, error = function(e) {
      # If intersection fails, return 0
      0
    })
  })
  
  # Return the tag index with maximum overlap area
  tag_indices[which.max(overlap_areas)]
})

# Step 3: Transfer the tags
twelve_geometry$transferred_tag <- tagged$Valley[best_tag_idx]

st_write(twelve_geometry, file.path('.', 'data', 'check_tags.gpkg'), append = F)

rm(overlaps, tagged)
### now union

twelve_tagged <- twelve_geometry |>
  filter(!is.na(transferred_tag)) |>
  group_by(transferred_tag) |>
  summarize(geometry = st_union(geom)) |>
  st_make_valid() |>
  st_cast('POLYGON') |>
  st_make_valid() 

twelve_tagged <- st_transform(twelve_tagged, 5070)
twelve_tagged <- smoothr::fill_holes(twelve_tagged, threshold = units::set_units(15000, m^2))

sts <- tigris::states() |>
  filter(!NAME %in% c('Alaska', 'Hawaii') ) |>
  st_transform(st_crs(twelve_tagged)) |>
  st_union()

twelve_tagged <- st_intersection(twelve_tagged, sts)
rm(sts)

# now remove mountains again just in case

mountains <- st_read(file.path('.', 'mountains', 'mountains.shp')) |>
  st_transform(5070) |>
  st_union() 
twelve_tagged <- st_difference(twelve_tagged, mountains)
rm(mountains)

st_write(twelve_tagged, file.path('.', 'data', 'Named_Valleys.gpkg'), append = F)
