load_solar_worldclim <- function() {
paths <- paste("data/wc2.1_2.5m_srad/", list.files("data/wc2.1_2.5m_srad/"), sep="")
paths_csv <- grep("csv", paths)

if (length(paths_csv) > 0){
  print("Data already compiled, can now read csv below")
}

if (length(paths_csv) == 0){
solar_worldclim <- raster::stack(paths)
new_solar_worldclim  <- raster::projectRaster(solar_worldclim, au_map) 
coordinates <- select(location_of_sites, longitude, latitude)
terra::extract(x = new_solar_worldclim,  y = sp::SpatialPoints(coordinates),method = "simple") %>% 
  as_tibble() %>% 
  mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>% 
  gather(key,value,-longitude, -latitude, -ID) %>%
  mutate(key = str_sub(key, -2)) %>%
  rename(month = key) -> extracted_solar

write_csv(extracted_solar,"data/wc2.1_2.5m_srad/solar_wordlclim.csv")
}
}
         