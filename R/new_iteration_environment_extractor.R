
extract_compile_data <- function(directory, data_source = c("elevation","WC","Envirem","Terraclim"), locations_contained){
  
  if(data_source == "WC"){
    result <- extract_compile_WC_data(directory, locations_contained)
    return(result)
  }
  if(data_source == "Envirem"){
    extract_compile_ENVIREM_data(directory, locations_contained)
    return(result)
  }  
  if(data_source == "Terraclim"){
    extract_compile_ENVIREM_data(directory, locations_contained)
    return(result)
  }
}


extract_compile_WC_data <- function(directory, locations_contained) {
  
  
    folder <- directory

    paths <- list.files(directory, full.names = TRUE)
    paths <- paths[grep(".tif", paths, fixed=TRUE)]
    raster_object <- raster::stack(paths)
    
    locations_contained <- locations_contained %>%
      distinct(latitude, longitude, .keep_all = TRUE)
    
    map(.x = seq(length(names(raster_object))), .f=extract_by_layer_WC, raster_object, locations_contained) %>%
      bind_rows() -> extracted_object
    extracted_object %>%
      filter(is.na(value))%>%
      distinct(ID, .keep_all = T) -> extracted_object_NA
    
    map(.x = seq(length(names(raster_object))), .f=iterate_buffer_WC, raster_object, extracted_object_NA) %>%
      bind_rows() -> extracted_NA_values
    
    extracted_object%>%
      drop_na(value) %>%
      mutate(buffer_extracted = "no") %>%
      bind_rows(extracted_NA_values) -> extracted_raster_object
    return(extracted_raster_object)
  }
  

extract_by_layer_WC <- function(layer, raster_object, locations_contained){
  
  i = layer
  new_raster_object  <- raster::projectRaster(raster_object[[i]], au_map) 
  coordinates <- select(locations_contained, longitude, latitude)
  
  if("wc2.1_30s_elev" %in% names(raster_object)){
    raster::extract(x = new_raster_object,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
      as_tibble() %>% 
      mutate(ID = locations_contained$ID,longitude = locations_contained$longitude, latitude = locations_contained$latitude) %>%
      mutate(month = NA)%>%
      mutate(env_name = str_sub(names(new_raster_object),start = -4, end = -1))
      } else{
  
  raster::extract(x = new_raster_object,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = locations_contained$ID,longitude = locations_contained$longitude, latitude = locations_contained$latitude) %>%
    mutate(month = str_sub(names(new_raster_object),-2))%>%
    mutate(env_name = str_sub(names(new_raster_object),start = -7, end = -4))
}
}


iterate_buffer_WC <- function(layer, raster_object, extracted_object_NA){
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  i = layer
  new_raster_object  <- raster::projectRaster(raster_object[[i]], au_map) 
  
  if( "wc2.1_30s_elev" %in% names(raster_object)){
    raster::extract(x = new_raster_object,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
      as_tibble() %>% 
      mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude) %>%
      mutate(month = NA)%>%
      mutate(env_name = str_sub(names(new_raster_object),start = -4, end = -1))
  } else{
    
    raster::extract(x = new_raster_object,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>% 
    mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude)  %>%
    mutate(month = str_sub(names(new_raster_object),-2)) %>%
    mutate(env_name = str_sub(names(new_raster_object),start = -7, end = -4))%>%
    mutate(buffer_extracted = "yes")
  }
    }


extract_compile_ENVIREM_data <- function(directory) {
  
  folder <- directory
  list.files(folder)
  
  paths <- paste(directory, "/" , list.files(directory), sep="")
  paths <- paths[grep(".tif", paths, fixed=TRUE)]
  raster_object <- raster::stack(paths)
  
  map(.x = seq(length(names(raster_object))), .f=extract_by_layer_ENVIREM, raster_object) %>%
    bind_rows() -> extracted_object
  
  extracted_object %>%
    filter(is.na(value))%>%
    distinct(ID, .keep_all = T) -> extracted_object_NA
  
  map(.x = seq(length(names(worldclim))), .f=iterate_buffer_ENVIREM, raster_object, extracted_object_NA) %>%
    bind_rows() -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    bind_rows(extracted_NA_values) -> extracted_ENVIREM
  return(extracted_worldclim)
}



extract_by_layer_ENVIREM <- function(layer, raster_object){
  i = layer
  new_raster_object  <- raster::projectRaster(raster_object[[i]], au_map) 
  coordinates <- select(location_of_sites, longitude, latitude)

  raster::extract(x = new_raster_object,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    mutate(month = i)%>%
    mutate(env_name = str_split(names(new_raster_object), "_")[[1]][3])
}


iterate_buffer_ENVIREM <- function(layer, ENVIREM, extracted_object_NA){
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  i = layer
  new_raster_object  <- raster::projectRaster(raster_object[[i]], au_map) 
  
  raster::extract(x = new_raster_object,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>% 
    mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude)  %>%
    mutate(month = i) %>%
    mutate(env_name = str_split(names(new_raster_object), "_")[[1]][3])
}




