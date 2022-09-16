load_reg_depth <- function(){
  if(!file.exists("data/depth_of_regolith/reg_depth.RDS")){
reg_depth <- raster::stack("data/depth_of_regolith/DER_000_999_EV_N_P_AU_NAT_C_20150601_fixed.tif")

raster::crs(reg_depth) <- raster::crs(au_map)
coordinates <- dplyr::select(location_of_sites, longitude, latitude)
raster::extract(x = reg_depth,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
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
  raster::extract(x = reg_depth[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
    as_tibble() %>%
    mutate(key = names(reg_depth[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, buffer_extracted = "yes")
}

map(.x = seq(length(names(reg_depth))), .f=iterate_buffer) %>%
  bind_rows() -> extracted_NA_values

extracted_object%>%
  drop_na(value) %>%
  mutate(buffer_extracted = "no") %>%
  bind_rows(extracted_NA_values) -> reg_depth

saveRDS(reg_depth, "data/depth_of_regolith/reg_depth.RDS")
  } else {
    readRDS("data/depth_of_regolith/reg_depth.RDS")
  }
}
