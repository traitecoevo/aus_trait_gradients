iterate_buffer_WC <- function(layer, worldclim, extracted_object_NA){
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  i = layer
  new_worldclim  <- raster::projectRaster(worldclim[[i]], au_map) 
  
  raster::extract(x = new_worldclim,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>% 
    mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude)  %>%
    mutate(month = str_sub(names(new_worldclim),-2)) %>%
    mutate(env_name = str_sub(names(new_worldclim),start = -7, end = -4))
}


extract_by_layer_WC <- function(layer, worldclim){
  i = layer
  new_worldclim  <- raster::projectRaster(worldclim[[i]], au_map) 
  coordinates <- select(location_of_sites, longitude, latitude)
  browser()
  
  raster::extract(x = new_worldclim,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    mutate(month = str_sub(names(new_worldclim),-2))%>%
    mutate(env_name = str_sub(names(new_worldclim),start = -7, end = -4))
}

extract_compile_WC_data <- function(directory) {
  
  browser()

  folder <- directory
  list.files(folder)
  
  paths <- paste(directory, "/" , list.files(directory), sep="")
  paths <- paths[grep(".tif", paths, fixed=TRUE)]
  worldclim <- raster::stack(paths)

  map(.x = seq(length(names(worldclim))), .f=extract_by_layer_WC, worldclim) %>%
      bind_rows() -> extracted_object
  
  extracted_object %>%
      filter(is.na(value))%>%
      distinct(ID, .keep_all = T) -> extracted_object_NA
    
  map(.x = seq(length(names(worldclim))), .f=iterate_buffer_WC, worldclim, extracted_object_NA) %>%
      bind_rows() -> extracted_NA_values
    
  extracted_object%>%
      drop_na(value) %>%
      mutate(buffer_extracted = "no") %>%
      bind_rows(extracted_NA_values) -> extracted_worldclim
    
    return(extracted_worldclim)
  }

iterate_buffer_ENVIREM <- function(layer, worldclim, extracted_object_NA){
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  i = layer
  new_worldclim  <- raster::projectRaster(worldclim[[i]], au_map) 
  
  raster::extract(x = new_worldclim,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>% 
    mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude)  %>%
    mutate(month = str_sub(names(new_worldclim),-2)) %>%
    mutate(env_name = str_sub(names(new_worldclim),start = -7, end = -4))
}


extract_by_layer_ENVIREM <- function(layer, worldclim){
  i = layer
  new_worldclim  <- raster::projectRaster(worldclim[[i]], au_map) 
  coordinates <- select(location_of_sites, longitude, latitude)
  browser()
  
  raster::extract(x = new_worldclim,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    mutate(month = str_sub(names(new_worldclim),-2))%>%
    mutate(env_name = str_sub(names(new_worldclim),start = -7, end = -4))
}

extract_compile_ENVIREM_data <- function(directory) {
  
  browser()
  
  folder <- directory
  list.files(folder)
  
  paths <- paste(directory, "/" , list.files(directory), sep="")
  paths <- paths[grep(".tif", paths, fixed=TRUE)]
  worldclim <- raster::stack(paths)
  
  map(.x = seq(length(names(worldclim))), .f=extract_by_layer_ENVIREM, worldclim) %>%
    bind_rows() -> extracted_object
  
  extracted_object %>%
    filter(is.na(value))%>%
    distinct(ID, .keep_all = T) -> extracted_object_NA
  
  map(.x = seq(length(names(worldclim))), .f=iterate_buffer_ENVIREM, worldclim, extracted_object_NA) %>%
    bind_rows() -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    bind_rows(extracted_NA_values) -> extracted_ENVIREM
  
  return(extracted_worldclim)
}









extract_compile_data <- function(directory, data_source = c("WC","Envirem","Terraclim")){
  browser()
  
  if(data_source == "WC"){
    extract_compile_WC_data(directory)
  }
  if(data_source == "Envirem"){
    extract_compile_WC_data(directory)
    
  }  
  if(data_source == "Terraclim"){
    
  }
}
