#' a wrapper around terra::makeTiles for setting the domain of a project
#' 
#' @param dirin the input directory, including path, with the dataset
#' @param dirout the output directory, including path, for the subset dataset to go
#' @param grid spatvector defining the tiles, should contain a column containing grid cell names
#' @param fnames name of column containing filenames in the grid
mason <- function(dirin, dirout, grid, fnames, ...){

  # identify the paths to the rasters
  dirin <- sub('/$', '', dirin)
  paths2rast <- file.path(dirin, list.files(path = paste0(dirin, '/'), recursive = T, ...))

  # create and crop the virtual raster
  virtualRast <- terra::vrt(paths2rast)
  virtualRast_sub <- terra::crop(virtualRast, terra::ext(grid))

  # set file names and write out product
  columns <- terra::values(grid)
  cellname <- columns[,fnames]
  dirout <- sub('/$', '', dirout)
  
  fname <- file.path(dirout, paste0(cellname, ".tif"))
  terra::makeTiles(virtualRast_sub, grid, filename = fname, na.rm = F)
  
  rm(virtualRast_sub)
  gc()

}


#' try to download from Kew again, with just the binomial
#' 
#' several infraspecies fail from POWO, retry the query with just the base name
#' @param x output from the first step of 'powo_searcher'
try_again <- function(x) {
  q <- x[['query']]
  only_binomial <- unlist(stringr::str_split(q, pattern = " "))
  only_binomial <- paste(only_binomial[1], only_binomial[2])
  
  results <- search_powo(only_binomial)
  return(results)
}

#' collect the results of a successful powo search
#' 
#' @param x a successful powo search query
result_grabber <- function(x) {
  # subset the appropriate data frame, there is one if clean data were entered,
  # and two if a synonym was entered.
  results <- x[['results']]
  
  clean <- which(unlist(lapply(results, `[`, 'accepted')) == T)
  resultT <- data.frame(results[clean])
  
  # the main variables which all results should have.
  family <- resultT$family
  authority <- resultT$author
  full_name <- resultT$name
  split_name <- unlist(stringr::str_split(full_name, pattern = " "))
  genus <- split_name[1]
  epithet <- split_name[2]
  
  # the infra species information if relevant.
  if (length(split_name) > 2) {
    infraspecies <- split_name[length(split_name)]
  } else {
    infraspecies <- NA
  }
  
  if (length(split_name) > 2) {
    infrarank <-
      stringr::str_extract(full_name, ' var\\. | spp\\. | subsp\\. ') |>
      stringr::str_trim(side = 'both')
  } else {
    infrarank <- NA
  }
  
  if (is.null(authority)){
    
    res <- autonym(genus, epithet, infrarank, infraspecies)
    name_authority <- res[[1]]
    authority <- res[[2]]
    
  } else{name_authority = paste(full_name, authority)}
  
  taxonomic_info <- data.frame(
    family,
    'name_authority' = name_authority,
    full_name,
    genus,
    epithet,
    infrarank,
    infraspecies,
    authority
  )
  
  return(taxonomic_info)
}

#' query plants of the world online for taxonomic information
#' 
#' a wrapper for kewr::search_powo
#' @param x a vector of species names to submit, these should have clean spelling
#' notes: results are observed to fail for valid infraspecies on Kew's end, and they seem not
#' to mention valid infraspecies. 
powo_searcher <- function(x) {
  query_results <- search_powo(x)
  
  if (is.null(query_results[["results"]])) {
    second_try <- try_again(query_results)
    
    if (is.null(second_try[["results"]])) {
      taxonomic_info <- data.frame(
        family = as.character(),
        name_authority = as.character(),
        full_name = as.character(),
        genus = as.character(),
        epithet = as.character(),
        infrarank = as.character(),
        infraspecies = as.character(),
        authority = as.character()
      )
      taxonomic_info[1,] <- 'NOT FOUND'
      
    } else {
      results_to_process <- second_try
    }
    
  } else {
    results_to_process <- query_results
  }
  
  # this is the end of the process, return empty results without error, or real results
  if (exists('taxonomic_info')) {
    return(cbind(query = x, taxonomic_info))
  } else {
    taxonomic_info <- result_grabber(results_to_process)
    return(cbind(query = x, taxonomic_info))
  }
}

#' retrieve author results for autonyms
#' 
#' this function is interal to result_grabber
#' @param  genus derived from result_grabber
#' @param  epithet derived from result_grabber
#' @param  infrarank derived from result_grabber
#' @param  infraspecies derived from result_grabber
autonym <- function(genus, epithet, infrarank, infraspecies){
  
  authority_hunt <- search_powo(paste(genus, epithet))
  
  results <- authority_hunt[['results']]
  clean <- which(unlist(lapply(results, `[`, 'accepted')) == T)
  auth_hunt <- data.frame(results[clean])
  authority <- auth_hunt$author
  
  name_authority <- paste(genus, epithet, authority, infrarank, infraspecies)
  return(list(name_authority, authority))
}

#' check that genera and specific epithets are spelled (almost) correctly
#' 
#' @param x a vector of species names
spell_check <- function(x) {
  
  pieces <- unlist(stringr::str_split(x, pattern = " "))
  genus <- pieces[1] ; species <- pieces[2]
  binom <- paste(genus, species)
  
  # infraspecies should be found without much hassle due to their length
  if(length(pieces) == 4){
    infras <- na.omit(epiLKPtab)
    full_name <- paste(genus, species, 
                       stringr::str_replace(pieces[3], 'ssp\\.|ssp', 'subsp.'), pieces[4])
    
    if (any(grep( x = infras$scientificName, pattern = full_name, fixed = T))) {
      return(data.frame(Query = x, Result = full_name, Match = 'exact'))
    } else {
      
      infras <- na.omit(epiLKPtab)
      full_name <- paste(genus, species, 
                         stringr::str_replace(pieces[3], 'ssp\\.|ssp', 'subsp.'), pieces[4])
      infraspecies_name <-
        infras[which.min(adist(full_name, infras$scientificName)), 'scientificName'] |> as.character()
      return(data.frame(Query = x, Result = infraspecies_name, Match = 'fuzzy'))
    } 
    
  # species can become difficult due to their short  names, e.g. 'Poa annua'
  } else {
  
    if (any(grep( x = cla$scientificName,pattern = binom, fixed = T))) {
      return(data.frame(Query = x, Result = binom, Match = 'exact'))
    } else{
      # try and determine which piece is incorrect.
      
      # subset datasets to query each name component separately
      genus2char <- stringr::str_extract(genus, '[A-Z][a-z]{1}')
      species3char <- stringr::str_extract(species, '[a-z]{3}')
      gen_strings <-
        dplyr::filter(genLKPtab, Grp == genus2char) |> dplyr::pull(strings)
      spe_strings <-
        dplyr::filter(speLKPtab, Grp == species3char) |> dplyr::pull(strings)
      
      # check to see if both genus and species are clean
      if (any(grep(x = gen_strings, pattern = paste0('^', genus, '$')))) {
        clean_genus_Tag <- genus
      } else {
        possible_genus_Tag <-
          gen_strings[which.min(adist(genus, gen_strings))]
      }
      
      # is species clean
      if (any(grep(x = spe_strings, pattern = paste0('^', species, '$')))) {
        clean_species_Tag <- species
      } else {
        possible_species_Tag <-
          spe_strings[which.min(adist(species, spe_strings))]
      }
      
      # if both the genus and species name are present, we could be missing it from the DB
      if (exists('clean_genus_Tag') & exists ('clean_species_Tag'))
      {
        return(data.frame(
          Query = x, Result = binom, Match = 'Suspected missing from ref DB'))
      } else { # if one is not clean search them with the 'cleaned' up versions
        combos <- ls()[grep(ls(), pattern = 'Tag')]
        search_q <-
          combos[c(grep(combos, pattern = 'genus'),
                   grep(combos, pattern = 'species'))]
        search_nom <- paste(unlist(mget(search_q)), collapse = " ")
        
        if (any(grep(x = epiLKPtab$scientificName, pattern = search_nom, fixed = T))) {
          
          return(data.frame(Query = x, Result = search_nom, Match = 'fuzzy'))
          
        } else{
          possible_binomial <- 
            epiLKPtab[which.min(adist(search_nom, epiLKPtab$scientificName)), 'scientificName'] |>
              as.character()
          return(data.frame(Query = x, Binomial = possible_binomial, Match = 'fuzzy'))
        }
      }
    }
  }
}


#' make a quick county dot map to display the location of the collection
#' 
#' @param x an sf dataframe of coordinates to make maps for, requires collection number and spatial attributes
#' @param path a directory to store the map images in before merging
#' @param collection_col column specify the collection number or other UNIQUE id for the collection

map_maker <- function(x, states, path, collection_col){
  
  if(st_crs(x) == sf::st_crs(states)) {
    pts } else {pts <- sf::st_transform(x, sf::st_crs(states))}
  
  ints <- sf::st_intersection(pts, states)
  
  focal_state <- dplyr::filter(states, STATE == ints$STATE)
  
  p <- ggplot() + 
    geom_sf(data = focal_state, fill = NA, color = 'grey15') + 
    geom_sf(data = pts, size = 0.5) +
    theme_void() 
  
  fname <- file.path(path, 'maps', paste0('map_', sf::st_drop_geometry(x[1,'collectionNo']), '.png'))
  ggsave(filename =  fname, plot = p, device = 'png', dpi = 300, width = 1, height = 1, units = 'in', 
         bg = 'transparent')
}


#' gather political site information
#' 
#' this function grabs information on the state, county, and township of collections
#' @param x an sf data frame of collection points
#' @param y a column which unambiguously identifies each collection
political_grabber <- function(x, y) {
  
  y_quo <- rlang::enquo(y)
  
  political <- sf::st_read('../geodata/political/political.shp', quiet = T)
  allotment <- sf::st_read('../geodata/allotments/allotments.shp', quiet = T)
  plss <- sf::st_read('../geodata/plss/plss.shp', quiet = T)
  ownership <- sf::st_read('../geodata/pad/pad.shp', quiet = T)
  
  # write attributes to data set
  
  x <- sf::st_join(x, political)
  x <- sf::st_join(x, allotment)
  x <- sf::st_join(x, ownership)
  
  x_plss <- sf::st_transform(x, sf::st_crs(plss))
  x_plss <- sf::st_join(x_plss, plss) |>
    sf::st_drop_geometry() %>% 
    dplyr::select(y, trs)
  
  x_vars <- dplyr::left_join(x, x_plss, by = y) |> 
    dplyr::mutate(Country = 'U.S.A.') |> 
    dplyr::relocate(any_of(c('Country', 'State', 'County', 'Mang_Name', 'Unit_Nm', 'trs')),
             .before = geometry) |>
    dplyr::distinct(.keep_all = T) |> # with large enough sample size some points fall on an exact border
    dplyr::group_by( .data[[y]] ) |>
    dplyr::slice_head(n = 1) |> 
    dplyr::ungroup()
  
  x_vars <- x_vars %>% 
    dplyr::mutate(
      Gen = paste0(
        Country, ', ', State, ', ', County, ' Co., ', Mang_Name, " ", Unit_Nm, " ", trs), 
      Gen = stringr::str_replace_all(Gen, "NA", ""), 
      Gen = stringr::str_replace_all(Gen, "  ", ""),
      Gen = stringr::str_trim(Gen),
      Gen = stringr::str_remove(Gen, ",$"), 
      .before = geometry)
  
  return(x_vars)
  
  rm(political, allotment, plss, ownership)
}

#' gather place and site information
#' 
#' this function grabs information on the state, county, and township of collections
#' @param x an sf data frame of collection points
#' @param y a column which unambiguously identifies each collection
place_grabber <- function(x) {
  
  mountains <- sf::st_read('../geodata/mountains/mountains.shp', quiet = T)
  places <- sf::st_read('../geodata/places/places.shp', quiet = T)
  
  x <- sf::st_join(x, mountains)
  
  nearest_place <- places[sf::st_nearest_feature(x, places),]
  distance <- round(
    as.numeric(
      sf::st_distance(x, nearest_place, by_element = T)),
    -2) / 1000 # distance in kilometers
  
  azimuth <- nngeo::st_azimuth(
    nearest_place, # from 
    x) |> round(0) # to
  
  places <- data.frame(
    'place' = sf::st_drop_geometry(nearest_place), distance, azimuth)  |>
    dplyr::rename(place = fetr_nm)
    
  x <- dplyr::bind_cols(x, places)|>
    dplyr::mutate(MapName = str_remove(MapName, ' \\(nn\\)')) |>
    dplyr::mutate(area_from =
                    paste0('From ', place, ' ', distance, ' km at ', azimuth, '°. ', 
                           MapName, '.'),
                  .before = geometry) |>
    dplyr::mutate(area_from = str_remove(area_from, ' NA[.]')) |>
    dplyr::select(-place, -distance, -azimuth, -MapName) 
  
  return(x)
}




#' gather physical characteristics of the site
#' 
#' this function grabs information on the elevation, azimuth, geomorphon, and geology of the site
#' @param x an sf data frame of collection points
#' @param y a column which unambiguously identifies each collection
physical_grabber <- function(x) {
  
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    return(df)
  }
  
  x_spat <- terra::vect(x)
  geology <- terra::vect('../geodata/geology/geology.shp')
  x_geo <- terra::extract(geology, x_spat)
  
  asp <- '../geodata/aspect/'; slo <-  '../geodata/slope'
  geo <-  '../geodata/geomorphons'; elev <-  '../geodata/elevation'
  
  paths2rast <- file.path(asp, list.files(path = paste0(asp, '/'), recursive = T))
  aspect <- terra::vrt(paths2rast)
  
  paths2rast <- file.path(slo, list.files(path = paste0(slo, '/'), recursive = T))
  slope <- terra::vrt(paths2rast)
  
  paths2rast <- file.path(geo, list.files(path = paste0(geo, '/'), recursive = T))
  geomorphon <- terra::vrt(paths2rast)
  
  paths2rast <- file.path(elev, list.files(path = paste0(elev, '/'), recursive = T))
  elevation <- terra::vrt(paths2rast)

  asp_val <- terra::extract(aspect, x_spat)
  names(asp_val) <- c('ID', 'aspect')
  slo_val <- terra::extract(slope, x_spat)

  geoLKPtab <- data.frame(unit = 1:10, geomorphon = c(
    'flat', 'peak', 'ridge', 'shoulder', 'spur', 
    'slope', 'hollow', 'footslope', 'valley', 'pit'
  ))
  
  geo_val <- terra::extract(geomorphon, x_spat)
  names(geo_val) <- c('ID', 'unit')
  geo_val <- dplyr::left_join(geo_val, geoLKPtab, by = 'unit') |>
    dplyr::select(-unit)
  
  ele_val <- terra::extract(elevation, x_spat)
  
  values <- data.frame(cbind(
    asp_val, 'slope' = slo_val[,2],  'elevation_m' = ele_val[,2]), 
    'geomorphon' = geo_val[,2], 'geology' = x_geo[,2])
  
  values$elevation_ft <- round(values$elevation_m * 3.28084, -1)
  values <- round_df(values, 0)
  
  el_cols <- c('elevation_m', 'elevation_ft')
  values[el_cols] <- lapply(values[el_cols], scales::comma)
  
  cols <- c('elevation_m', 'elevation_ft', 'aspect', 'slope', 
            'geomorphon', 'geology') 
  
  object <- dplyr::bind_cols(x, values) |> 
    dplyr::select(-ID) |>
    dplyr::relocate(tidyselect::all_of(cols), .before = geometry) |> 
    dplyr::mutate(
      physical_environ = 
        paste0('At ', elevation_ft, ' ft, on a ', geomorphon, ', ', slope,
               '° slo. and ', aspect, '° asp.; geology: ', geology, '.'
                                ), .before = geometry)
  
  return(object)
}



#' take international format date and make it written herbarium label format
#' 
#' @param x a data frame with dates
date2text <- function(x) {
  
  x1 <- lubridate::mdy(x)
  
  text <- paste0(lubridate::day(x1), # just grab day of month here
                 ' ',
                 lubridate::month(x1, abbr = T, label = T), # grab the abbreviation for the month here
                 ', ',
                 lubridate::year(x1)) # grab the year here
  return(text)
}

#' create herbarium format dates for specimens
#' 
#' this function will return up to two additional columns with x. 'coll_date_text' a text format for the date of collection and 'det_date_text" a shorter text format date for identification
#' @param x an (sf/tibble/) data frame with both the collection date
#' and determination date
#' @param coll_date date of collection, expected to be of the format 'MM/DD/YYYY', minor checks for compliance to the format will be carried out before returning an error.
#' @param det_date date of determination, same format and processes as above. 
#' @returns original data frame plus: x_dmy, x_day, x_month, x_year, x_text, det_date_text, columns for both parameters which are supplied as inputs
date_parser <- function(x, coll_date, det_date){
  
  coll_date_q <- enquo(coll_date)
  det_date_q <- enquo(det_date)
  
  names_v <- c('_dmy', '_day', '_mo',  '_yr', '_text')
  if(missing(det_date)){
    column_names <- c(coll_date, paste0(coll_date, names_v))
  } else {
    column_names <- c(coll_date, 
                      paste0(coll_date, names_v), 
                        det_date, 
                        paste0(det_date, names_v))
  }
  
  x_dmy <- x |> 
    mutate(across(.cols = c(!!coll_date_q, !!det_date_q), lubridate::mdy, .names = "{.col}_dmy")) |>
    mutate(across(ends_with('_dmy'), ~ lubridate::month(.), .names = "{.col}_mo"),
           across(ends_with('_dmy'), ~ lubridate::day(.), .names = "{.col}_day"),
           across(ends_with('_dmy'), ~ lubridate::year(.), .names = "{.col}_yr")) |>
    mutate(across(.cols = c(!!coll_date_q, !!det_date_q), date2text, .names = '{.col}_text')) |>
    rename_with( ~ stringr::str_remove(., '_dmy'), matches("_dmy_.*$")) |>
    relocate(any_of(column_names), .before = geometry)
  
  return(x_dmy)
}



#' this function parses coordinates from DMS to decimal degrees
#'
#'@param x an input data frame to apply operations too
#'@param lat a name of the column holding the latitude values
#'@param long a name of the colymn holding the longitude values
#'@param dms are coordinates in degrees minutes seconds? TRUE for yes, FALSE for decimal degrees
dms2dd <- function(x, lat, long, dms){
  
  # identify columns if they were not supplied
  if(missing(lat)){
    lat = colnames(dummy_pts)[grep('lat', colnames(dummy_pts))] }
  if(missing(long)){
    long = colnames(dummy_pts)[grep('long', colnames(dummy_pts))] }
  
  # test for DMS format if not supplied
  suppressWarnings(  if(missing(dms)){
    dms = long == parzer::parse_lon(long)
  })
  
  # convert dms to dd, or rename input columns to dd
  if(dms == T){
    x$latitude_dd = parzer::parse_lat(lat)
    x$longitude_dd = parzer::pase_lon(long)
  } else{
    colnames(x)[which(names(x) == lat)] <- 'latitude_dd'
    colnames(x)[which(names(x) == long)] <- 'longitude_dd'
  }
  # ensure the DD signs are appropriate for domain
  x$latitude_dd <- abs(x$latitude_dd)
  x$longitude_dd <- abs(x$longitude_dd) * -1
  
  # now overwrite the original DMS values in our exact formaty
  
  x$latitude_dms = paste0(
    'N ', parzer::pz_degree(x$latitude_dd),
    '°', round(parzer::pz_minute(x$latitude_dd), 2),
    "'", round(parzer::pz_second(x$latitude_dd), 2)
  )
  
  x$longitude_dms = paste0(
    'W ', parzer::pz_degree(x$longitude_dd), 
    '°', round(parzer::pz_minute(x$longitude_dd), 2), 
    "'", round(parzer::pz_second(x$longitude_dd), 2)
  )
  
  x$longitude_dms <- gsub('-', "", x$longitude_dms)
  
  return(x)
  
}


#' create an sf object row by row to reflect different datum
#' 
#' @param x a dataframe which has undergone dms2dd function
#' @param datum a column, holding the datum values for each observation, works for WGS84, NAD83, NAD27
coords2sf <- function(x, datum){
  
  if(missing(datum)){ # identify datum information
    datum = colnames(dummy_pts)[grep('datum', colnames(dummy_pts))] }
  
  if(length(datum) == 0){x$datum = 'WGS84'} # if column doesn't exist create
  
  dat_check <- function(x){if(grepl('nad.*27', ignore.case = T, x = x)) {
    x = "NAD27"} else if(grepl('nad.*83', ignore.case = T, x = x)) {
      x = 'NAD83'} else if(grepl('wgs', ignore.case = T, x = x)) {
        x = "WGS84"
      } else {x = 'WGS84'}
  }
  # now ensure the datum is appropriate for each point
  x$datum <- sapply(x$datum, dat_check)
  
  crs_lkp = data.frame(
    datum = c('NAD27', 'NAD83', 'WGS84'), 
    crs = c(4267, 4269, 4326))
  
  x <- dplyr::left_join(x, crs_lkp, by = 'datum') 
  
  # separate all points by there datum
  dat_list <- split(x, f = x$datum)
  dat_list <- purrr::map(dat_list, . %>%  
                           sf::st_as_sf(., coords = c(x = 'longitude_dd', y = 'latitude_dd'), 
                                        crs = .$crs[1], remove = F) %>% 
                           sf::st_transform(4326)) |>
    dplyr::bind_rows() %>% 
    dplyr::select(-crs) %>% 
    dplyr::mutate(datum = 'WGS84', .before = geometry)
  
  return(dat_list)
}
