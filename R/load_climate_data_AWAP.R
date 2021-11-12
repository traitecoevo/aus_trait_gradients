load_vp09_data <- function(path){
  vp09_path <- paste0("data/agcd/v1/vapourpres_h09_monthly/mean/r005/01month/",as.character(path))
  vp09<-raster::stack(vp09_path)
  new_VP09 <-
    raster::projectRaster(vp09, au_map) # harmonize the spatial extent and projection
  coordinates <- dplyr::select(location_of_sites, longitude, latitude)
  raster::extract(x = new_VP09,  y = sp::SpatialPoints(coordinates),method = "simple") %>% 
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
  raster::extract(x = new_VP09[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>%
    mutate(key = names(new_VP09[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, extracted = "yes")
}

map(.x = seq(length(names(new_VP09))), .f=iterate_buffer) %>%
  bind_rows() -> extracted_NA_values

extracted_object%>%
  drop_na(value) %>%
  mutate(extracted = "no") %>%
  bind_rows(extracted_NA_values)
}

load_vp15_data <- function(path){
  vp15_path <- paste0("data/agcd/v1/vapourpres_h15_monthly/",as.character(path))
  vp15<-raster::stack(vp15_path)
  new_VP15 <-
    raster::projectRaster(vp15, au_map) # harmonize the spatial extent and projection
  coordinates <- select(location_of_sites, longitude, latitude)
  raster::extract(x = new_VP15,  y = sp::SpatialPoints(coordinates),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>% 
    gather(key,value,-longitude, -latitude, -ID)
}

load_max_t_data <- function(path){
  max_t_path <- paste0("data/agcd/v1/monthly_tmax/",as.character(path))
  max_t<-raster::stack(max_t_path)
  new_max_t <-
    raster::projectRaster(max_t, au_map) # harmonize the spatial extent and projection
  coordinates <- select(location_of_sites, longitude, latitude)
  raster::extract(x = new_max_t,  y = sp::SpatialPoints(coordinates),method = "simple") %>%
    as_tibble() %>%
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    gather(key,value,-longitude, -latitude, -ID)
}

load_solar_data <- function(path){
  solar_path <- paste0("data/AWAP_solar/", path)
  solar<-raster::stack(solar_path)
  raster::crs(solar) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  new_solar <-
    raster::projectRaster(solar, au_map) # harmonize the spatial extent and projection
  coordinates <- select(location_of_sites, longitude, latitude)
  raster::extract(x = new_solar,  y = sp::SpatialPoints(coordinates),method = "simple") %>%
    as_tibble() %>%
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude, key = new_solar@data@names) 
}

load_rainfal_data <- function(path){
  rainfall_path <- paste0("data/agcd/v2/r005/01month/",as.character(path))
  rainfall<-raster::stack(rainfall_path)
  new_rainfall <-
    raster::projectRaster(rainfall, au_map) # harmonize the spatial extent and projection
  coordinates <- select(location_of_sites, longitude, latitude)
  raster::extract(x = new_rainfall,  y = sp::SpatialPoints(coordinates),method = "simple") %>% 
    as_tibble() %>% 
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>% 
    gather(key,value,-longitude, -latitude, -ID)
}
