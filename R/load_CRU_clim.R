load_CRU_clim <- function(directory, file){
  CRU_clim <- raster::brick(paste(directory, file, sep=""))
  files<-list.files(directory)
  if(!file.exists(paste(directory,str_sub(file, end=-4), ".RDS", sep=""))){
  new_CRU_clim <-
    raster::projectRaster(CRU_clim, au_map) # harmonize the spatial extent and projection
  saveRDS(new_CRU_clim, paste(directory,str_sub(file, end=-4), ".RDS", sep=""))
  } else{
    new_CRU_clim <- readRDS(paste(directory,str_sub(file, end=-4), ".RDS", sep=""))
  }
  coordinates <- select(location_of_sites, longitude, latitude)
  raster::extract(x = new_CRU_clim,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple")%>%
    as_tibble() %>%
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    pivot_longer(-c(ID,longitude,latitude)) %>%
    mutate(name = str_sub(name, start =2)) %>%
    separate(name, c("year","month","day"), sep="\\.")-> extracted_object
  
  extracted_object %>%
    filter(is.na(value))%>%
    distinct(ID, .keep_all = T) -> extracted_object_NA
  
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  
  iterate_buffer <- function(layer){
    i = layer
    raster::extract(x = new_CRU_clim[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
      as_tibble() %>%
      mutate(name = names(new_CRU_clim[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, buffer_extracted = "yes")
  }
  
  map(.x = seq(length(names(new_CRU_clim))), .f=iterate_buffer) %>%
    bind_rows()%>%
    mutate(name = str_sub(name, start =2)) %>%
    separate(name, c("year","month","day"), sep="\\.") -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    bind_rows(extracted_NA_values)
  
}
