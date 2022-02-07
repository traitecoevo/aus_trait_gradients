
load_climate_data <- function() {
  #### 01 Get climate data for Australia ####
  # Download bioclim data using library (raster)
  bioclim <- raster::getData("worldclim", var = "bio", res = 2.5)
  # Pick BIO1 (Mean Annual Temperature; T), BIO12 (Annual Precipitation; P) and BIO15 (Prec Seasonality (CV))
  bioclim <- bioclim[[c(1,5,6, 12, 15)]]
  names(bioclim) <- c("Temp_BC","max_temp_BC", "min_temp_BC", "Prec_BC", "Prec_CV_BC")
  
  #### 02 Get the climate data for Australia ####
  # Load Australia landmass binary map
  au_map <- raster::raster("data/australia.tif")
  # 
  new.bioclim <-
    raster::projectRaster(bioclim, au_map) # harmonize the spatial extent and projection
  
  coordinates <- dplyr::select(location_of_sites, longitude, latitude)
  raster::extract(x = new.bioclim,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
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
    raster::extract(x = new.bioclim[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
      as_tibble() %>%
      mutate(key = names(new.bioclim[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, buffer_extracted = "yes")
  }
  
  map(.x = seq(length(names(new.bioclim))), .f=iterate_buffer) %>%
    bind_rows() -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    bind_rows(extracted_NA_values)

  }
