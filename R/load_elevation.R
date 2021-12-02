load_elevation <- function(){
  elevation <- raster::raster("data/elevation/wc2.1_5m_elev.tif")
  new_elevation <-
    raster::projectRaster(elevation, au_map) # harmonize the spatial extent and projection
  coordinates <- select(location_of_sites, longitude, latitude)
  terra::extract(x = new_elevation,  y = sp::SpatialPoints(coordinates),method = "simple")%>%
    as_tibble() %>%
    mutate(ID = location_of_sites$ID,longitude = location_of_sites$longitude, latitude = location_of_sites$latitude) %>%
    rename(elevation = value)
}