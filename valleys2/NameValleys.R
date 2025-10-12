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

################################################################################

# match two, multiple valley labels per watersheed simpply split by closest label. 

## these valleys get split by the voronoi, and names assigned based on voronoi intersections
matched_multi <- st_drop_geometry(valleys)[ lengths(ints) >= 2, 'ID']

valleys_multi_match <- valleys |>
  filter(ID %in% matched_multi) |>
  st_join(GeoNames_names, join =  st_contains)


valleys_multi_voronoi <- valleys |>
  filter(ID %in% matched_multi) |>
  st_intersection(total_voron_attributed) 

# now remove these old geometries from the data set and replace them with these. 

valleys_partial_label <- valleys |>
  filter(!ID %in% c(matched_single, matched_multi)) |>
  bind_rows(valleys_single_match, valleys_multi_voronoi) |>
  arrange(ID)

rm(matched_multi, matched_single, valleys_sub, valleys_multi_voronoi, valleys_single_match,
   ints, total_voron_attributed)

################################################################################

## investigate possible assignments

# we can use a couple lines of evidence for this
# 1) neighbors, the nearest neighbor is most likely to have the same name. 
# 2) Azimuth - do the valleys run in roughly the same direction? 
# 3) shared border, if they have multiple neighbors, which one do they share the
# longest border with? 



# identify the nearest labelled neighbor. 




## determine the azimuths for the valleys. 
get_polygon_azimuth <- function(polygon_sf) {
  # Get minimum rotated rectangle
  bbox <- st_minimum_rotated_rectangle(polygon_sf)
  
  # Extract coordinates
  coords <- st_coordinates(bbox)[,1:2]
  
  # Calculate azimuth of longest side
  # Get unique edges
  edge1 <- sqrt(sum((coords[1,] - coords[2,])^2))
  edge2 <- sqrt(sum((coords[2,] - coords[3,])^2))
  
  # Use the longer edge
  if(edge1 > edge2) {
    dx <- coords[2,1] - coords[1,1]
    dy <- coords[2,2] - coords[1,2]
  } else {
    dx <- coords[3,1] - coords[2,1]
    dy <- coords[3,2] - coords[2,2]
  }
  
  # Calculate azimuth (0-360, clockwise from north)
  azimuth <- (90 - atan2(dy, dx) * 180/pi) %% 180
  
  return(azimuth)
}

valleys_partial_label$azimuth <- sapply(st_geometry(valleys_partial_label), get_polygon_azimuth)


# Function to get neighbor features for imputation
get_neighbor_features <- function(sf_object, label_column, azimuth_column) {
  
  # Create neighbor list
  nb <- poly2nb(sf_object, queen = TRUE)
  
  # For each polygon
  neighbor_features <- lapply(1:nrow(sf_object), function(i) {
    
    # Skip if already labeled
    if(!is.na(sf_object[[label_column]][i])) {
      return(NULL)
    }
    
    # Get lag 1 neighbors
    lag1_neighbors <- nb[[i]]
    
    if(length(lag1_neighbors) == 0) return(NULL)
    
    # Get lag 2 neighbors
    lag2_neighbors <- unique(unlist(nb[lag1_neighbors]))
    lag2_neighbors <- lag2_neighbors[lag2_neighbors != i]  # Remove self
    lag2_neighbors <- setdiff(lag2_neighbors, lag1_neighbors)  # Remove lag1
    
    # Find labeled neighbors at each lag
    labeled_lag1 <- lag1_neighbors[!is.na(sf_object[[label_column]][lag1_neighbors])]
    labeled_lag2 <- lag2_neighbors[!is.na(sf_object[[label_column]][lag2_neighbors])]
    
    # Determine search depth
    if(length(labeled_lag1) > 0) {
      # Found at lag 1, also search lag 2
      all_candidates <- c(labeled_lag1, labeled_lag2)
      candidate_lags <- c(rep(1, length(labeled_lag1)), 
                          rep(2, length(labeled_lag2)))
    } else if(length(labeled_lag2) > 0) {
      # Only found at lag 2, search lag 3
      lag3_neighbors <- unique(unlist(nb[lag2_neighbors]))
      lag3_neighbors <- lag3_neighbors[lag3_neighbors != i]
      lag3_neighbors <- setdiff(lag3_neighbors, c(lag1_neighbors, lag2_neighbors))
      
      labeled_lag3 <- lag3_neighbors[!is.na(sf_object[[label_column]][lag3_neighbors])]
      
      all_candidates <- c(labeled_lag2, labeled_lag3)
      candidate_lags <- c(rep(2, length(labeled_lag2)), 
                          rep(3, length(labeled_lag3)))
    } else {
      # Search lag 3
      lag3_neighbors <- unique(unlist(nb[lag2_neighbors]))
      lag3_neighbors <- lag3_neighbors[lag3_neighbors != i]
      lag3_neighbors <- setdiff(lag3_neighbors, c(lag1_neighbors, lag2_neighbors))
      
      labeled_lag3 <- lag3_neighbors[!is.na(sf_object[[label_column]][lag3_neighbors])]
      
      if(length(labeled_lag3) == 0) return(NULL)
      
      all_candidates <- labeled_lag3
      candidate_lags <- rep(3, length(labeled_lag3))
    }
    
    # Build candidate dataframe
    candidates_df <- data.frame(
      polygon_id = i,
      candidate_id = all_candidates,
      lag = candidate_lags,
      candidate_label = sf_object[[label_column]][all_candidates]
    )
    
    # Calculate azimuth difference
    candidates_df$azimuth_diff <- abs(
      sf_object[[azimuth_column]][i] - 
        sf_object[[azimuth_column]][candidates_df$candidate_id]
    )
    
    # Adjust to 0-90 range (account for orientation)
    candidates_df$azimuth_diff <- pmin(
      candidates_df$azimuth_diff,
      180 - candidates_df$azimuth_diff
    )
    
    # Calculate shared border percentage (ONLY for lag 1 neighbors)
    focal_geom <- st_geometry(sf_object[i,])
    focal_perimeter <- st_length(st_boundary(focal_geom))
    
    candidates_df$shared_border_pct <- sapply(1:nrow(candidates_df), function(j) {
      
      # Only compute for lag 1
      if(candidates_df$lag[j] != 1) return(NA_real_)
      
      cand_id <- candidates_df$candidate_id[j]
      cand_geom <- st_geometry(sf_object[cand_id,])
      
      # Get intersection
      shared <- st_intersection(focal_geom, cand_geom)
      
      # Check if it's a line (shared border)
      if(length(shared) > 0 && 
         st_geometry_type(shared, by_geometry = FALSE) %in% 
         c("LINESTRING", "MULTILINESTRING")) {
        shared_length <- st_length(shared)
        return(as.numeric(shared_length / focal_perimeter))
      } else {
        return(0)
      }
    })
    
    return(candidates_df)
  })
  
  # Combine all results
  bind_rows(neighbor_features)
}

# all polygons which have neighbors
neighbor_data <- get_neighbor_features(valleys_partial_label, "name", "azimuth")



## now sample data and score it in qgis


st <- tigris::states() # only have road atlasses for the following states
st <- filter(st, STUSPS %in% c('CA', 'WA', 'NV', 'ID', 'MT', 'CO', 'UT')) |>
  st_combine() |>
  st_make_valid() |>
  st_transform(st_crs(valleys_partial_label))

vpl_int <- valleys_partial_label[ lengths(st_intersects(valleys_partial_label, st))> 0, ]

xy <- valleys_partial_label |>
  st_as_sf(coords = geom) |>
  st_point_on_surface() %>%
  mutate( 
    X = st_coordinates(.)[,1], 
    Y = st_coordinates(.)[,2]
  ) |>
  st_drop_geometry() |>
  select(ID, X, Y)

possible_truthing_data <- filter(neighbor_data, polygon_id %in% vpl_int$ID) |>
  left_join( 
    xy ,
    by = c('polygon_id' = 'ID'), 
    relationship = "many-to-many") 


dput(possible_truthing_data[1:100,])















# Create features for stratification
possible_truthing_data_strata <- possible_truthing_data |>
  group_by(polygon_id) |>
  summarize(
    X = first(X),
    Y = first(Y),
    n_candidates = n(),
    n_lag1 = sum(lag == 1),
    n_lag2 = sum(lag == 2),
    n_lag3 = sum(lag == 3),
    max_shared_border = max(shared_border_pct, na.rm = TRUE),
    has_shared_border = any(!is.na(shared_border_pct) & shared_border_pct > 0)
  )

# Use quantile-based sampling (always creates valid bins)
set.seed(123)

# Stratify on the key continuous variables
sample_ids <- possible_truthing_data_strata |>
  mutate(
    # Spatial quantiles (guarantees equal-ish groups)
    X_quart = ntile(X, 4),
    Y_quart = ntile(Y, 4),
    
    # Number of candidates quantiles
    n_cand_quart = ntile(n_candidates, 4),
    
    # Lag 1 presence (binary - most important)
    has_lag1 = n_lag1 > 0
  ) |>
  # Calculate border quartile separately to avoid size mismatch
  mutate(
    border_quart = if_else(
      !has_shared_border, 
      0, 
      ntile(max_shared_border, 3)
    )
  ) |>
  # Sample from combinations that exist - prioritize key variables
  group_by(X_quart, Y_quart, has_lag1, n_cand_quart) |>
  slice_sample(n = 1) |>
  ungroup()

# If we don't have 100 yet, add more focusing on border diversity
if(nrow(sample_ids) < 100) {
  remaining_needed <- 100 - nrow(sample_ids)
  
  additional_ids <- possible_truthing_data_strata |>
    filter(!polygon_id %in% sample_ids$polygon_id) |>
    mutate(
      X_quart = ntile(X, 4),
      Y_quart = ntile(Y, 4),
      n_cand_quart = ntile(n_candidates, 4),
      border_quart = if_else(
        !has_shared_border,
        0,
        ntile(max_shared_border, 3)
      )
    ) |>
    # Prioritize border diversity in second round
    group_by(X_quart, Y_quart, border_quart, n_cand_quart) |>
    slice_head(n = 1) |>  # Changed from slice_sample to slice_head
    ungroup() |>
    slice_sample(n = min(remaining_needed, n()))  # Sample from what we have
  
  sample_ids <- bind_rows(sample_ids, additional_ids)
}

sample_ids <- sample_ids |>
  slice_sample(n = min(100, n())) |>
  pull(polygon_id)

# Get the training sample
training_sample <- possible_truthing_data |>
  filter(polygon_id %in% sample_ids$polygon_id) |>
  st_as_sf(coords = c('X', 'Y'), crs = 5070)|>
  select(polygon_id) |>
  distinct(polygon_id, .keep_all = TRUE)

to_search <- valleys_partial_label[ unlist(st_intersects(training_sample, valleys_partial_label)), ]

st_write(to_search, 'polygons2tag.gpkg')

write.csv(st_drop_geometry(to_search) |> mutate(REAL_NAME = ""), 'groundtruth2tag.csv', row.names = F)
