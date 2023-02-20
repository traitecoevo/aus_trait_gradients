list.files("data/bulk_density")

load_bulk_density <- function(files, depth) {
  
  bulk_dens <- raster::stack(paste("data/bulk_density/", files, sep=""))
  
  raster::crs(bulk_dens) <- raster::crs(au_map)
  coordinates <- dplyr::select(location_of_sites, longitude, latitude)
  raster::extract(x = bulk_dens,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>% 
    gather(key,value,-longitude, -latitude, -ID) -> extracted_object
  
  extracted_object %>%
    filter(is.na(value))%>%
    distinct(ID, .keep_all = T) -> extracted_object_NA
  
  extracted_object_NA %>%
    select(longitude, latitude)  -> extracted_object_NA_coordinates
  
  
  iterate_buffer <- function(layer){
    i = layer
    raster::extract(x = bulk_dens[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
      as_tibble() %>%
      mutate(key = names(bulk_dens[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, buffer_extracted = "yes")
  }
  
  map(.x = seq(length(names(bulk_dens))), .f=iterate_buffer) %>%
    bind_rows() -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    mutate(depth = depth) %>%
    bind_rows(extracted_NA_values)
}

compile_bulk_density <- function(){
  depth <- c(5,15,30,60,100,200)
  files <- list.files("data/bulk_density")
  
  values <- purrr::map2(files, depth, load_bulk_density) %>%
    bind_rows() %>%
    saveRDS("data/bulk_density/bulk_density.RDS")
}

