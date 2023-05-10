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
    return(taxonomic_info)
  } else {
    taxonomic_info <- result_grabber(results_to_process)
    return(taxonomic_info)
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


