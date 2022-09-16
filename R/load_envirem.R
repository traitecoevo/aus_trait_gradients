load_envirem <- function(directory, name_of_csv) {
  paths <- paste(directory, list.files(directory), sep="")
  paths_csv <- grep("csv", paths)
  
  if (length(paths_csv) > 0){
    print("Data already compiled, can now read csv below")
  }
  
  if (length(paths_csv) == 0){
    worldclim <- raster::stack(paths)
    new_worldclim  <- raster::projectRaster(worldclim, au_map) 
    coordinates <- select(location_of_sites, longitude, latitude)
    raster::extract(x = new_worldclim,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
        as_tibble() %>% 
        mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude)  -> extracted_object
    
    
    
    extracted_object %>%
      filter(is.na(value))%>%
      distinct(ID, .keep_all = T) -> extracted_object_NA
    
    extracted_object_NA %>%
      select(longitude, latitude)  -> extracted_object_NA_coordinates
    
    raster::extract(x = new_worldclim,  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
        as_tibble() %>% 
        mutate(ID = extracted_object_NA$ID,longitude = extracted_object_NA$longitude, latitude = extracted_object_NA$latitude) -> extracted_NA_values
   
    extracted_object%>%
      drop_na(value) %>%
      mutate(buffer_extracted = "no") %>%
      bind_rows(extracted_NA_values) -> extracted_worldclim
    
    csv_path <- paste(directory, name_of_csv, ".csv", sep="")
    write_csv(extracted_worldclim,csv_path)
  }
}

