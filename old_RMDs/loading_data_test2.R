trait_data <- woody_field_traits_georef
list.files(paste("data/climate_data/WC/",list.files("data/climate_data/WC/"),sep = "")[1], full.names = TRUE)%>%
  terra::rast() -> key_raster  

target_CRS <- terra::crs(key_raster)  

australia <- terra::vect("data/AUS_2021_AUST_SHP_GDA2020/", crs=target_CRS)
ext <- terra::ext(australia)
crs <- terra::crs(australia)
trait_data %>%
  group_by(longitude, latitude,ID) %>%
  nest() %>%
  ungroup() %>% 
  dplyr::select(-data)-> trait_data_coords


list.files(paste("data/climate_data/WC/",list.files("data/climate_data/WC/"),sep = ""), full.names = TRUE)[1:10]%>%
  terra::rast() %>%
  terra::crop(australia, mask = TRUE) -> raster_WC

list.files(paste("data/climate_data/wc2.1_30s_bio/",sep = ""), full.names = TRUE)[c(7,8,9,14)] %>%
  terra::rast() %>%
  terra::crop(australia, mask = TRUE)  -> raster_bioclim

raster_clim <- c(raster_WC, raster_bioclim)

list.files(paste("data/climate_data/envirem/",sep = ""), full.names = TRUE)[c(1,2,3,7)] %>%
  terra::rast() %>%
  terra::crop(australia, mask = TRUE) -> raster_envirem

list.files(paste("data/climate_data/VPD_Chelsa/",sep = ""), full.names = TRUE)%>%
  terra::rast() %>%
  terra::crop(australia, mask = TRUE) -> raster_Chelsa

c(raster_clim, raster_envirem, raster_Chelsa)-> raster_stack

extract_function <- function(stack){
  browser()
  terra::extract(x = stack,  y = sp::SpatialPoints(coords = cbind(coords$longitude, coords$latitude), proj4string=sp::CRS(raster_stack)),method = "simple", cellnum = TRUE) %>%
    as_tibble() %>%
    mutate(ID = trait_data_coords$ID, latitude = trait_data_coords$latitude, longitude = trait_data_coords$longitude)-> extracted_values_tmp
  NA_coords <- extracted_values_tmp %>%
    filter(if_any(!c(ID), ~(is.na(.))))
  
  new_coords <- seegSDM::nearestLand(NA_coords %>% dplyr::select(longitude,latitude), stack[[1]], max_distance = 10000) %>%
    as_tibble() %>%
    mutate(ID = NA_coords$ID) %>%
    drop_na()
  terra::extract(x = stack,  y = sp::SpatialPoints(coords = cbind(new_coords$x, new_coords$y), proj4string=sp::CRS(raster_stack)),method = "simple", cellnum = TRUE) %>%
    as_tibble() %>%
    mutate(ID = new_coords$ID, latitude = new_coords$y, longitude = new_coords$x)-> extracted_values_tmp_new
  
  extracted_values_tmp%>%
    filter(if_any(!c(ID, latitude, longitude,cells), ~(!is.na(.)))) %>%
    bind_rows(extracted_values_tmp_new) -> extracted_values[["WC"]]
}
walk(lis[[2]], extract_function)

terra::app()

lis <- list(raster_WC, raster_envirem, raster_Chelsa)
extracted_values <- list()