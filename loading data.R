library("tidyverse")

load_climate_data <- function(trait_data){

australia <- terra::rast("data/australia.tif")
ext <- terra::ext(australia)

trait_data %>%
  group_by(longitude, latitude,ID) %>%
  nest() %>%
  ungroup() -> trait_data_coords


coords <-  trait_data_coords[, c('longitude','latitude')] 

list.files(paste("data/climate_data/WC/",list.files("data/climate_data/WC/"),sep = ""), full.names = TRUE)[1:10] %>%
  terra::rast() %>%
  raster::crop(ext) %>%
  raster::stack() -> raster_WC

new_coords_WC <- seegSDM::nearestLand(coords, raster_WC[[1]], max_distance = 10000) %>%
  as_tibble() %>%
  mutate(ID = trait_data_coords$ID) %>%
  drop_na()
  
list.files(paste("data/climate_data/envirem/",sep = ""), full.names = TRUE)[c(3,7)] %>%
  terra::rast() %>%
  raster::crop(ext) %>%
  raster::stack()-> raster_envirem

new_coords_envirem <- seegSDM::nearestLand(coords, raster_envirem[[1]], max_distance = 10000) %>%
  as_tibble() %>%
  mutate(ID = trait_data_coords$ID) %>%
  drop_na()

raster_WC %>%
  raster::stack(raster_envirem) -> raster_stack

lis <- list(raster_WC, raster_envirem)

extracted_values <- list()

for(i in 1:length(lis)){
  if(i == 1){
    raster::extract(x = raster_stack[[1:raster::nlayers(raster_WC)]],  y = sp::SpatialPoints(coords = cbind(new_coords_WC$x, new_coords_WC$y), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>% 
      as_tibble() %>%
      mutate(ID = new_coords_WC$ID)-> extracted_values[["WC"]]    
  }
  if(i == 2){
      raster::extract(x = raster_stack[[(raster::nlayers(raster_WC) + 1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem))]],  y = sp::SpatialPoints(coords = cbind(new_coords_envirem$x, new_coords_envirem$y), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>% 
        as_tibble() %>%
      mutate(ID = new_coords_envirem$ID) -> extracted_values[["Envirem"]]
  }
}


extracted_values %>%
  purrr::reduce(left_join) %>%
  rename_with(~str_remove(string = ., pattern = "wc2.1_30s_"), .cols = starts_with("wc")) %>%
  rename_with(~str_remove(string = ., pattern = "current_30arcsec_"), .cols = starts_with("current_30arcsec_")) %>%
  pivot_longer(-c(cells, ID)) %>% 
  separate(name, into = c("env_name", "month"), sep="_(?=[^_]+$)") %>%
  group_by(ID, env_name) %>%
  nest() %>%
  pivot_wider(names_from = env_name, values_from = data) %>%
  mutate(prec = map_dbl(prec, ~sum(.x$value))) %>%
  ungroup() %>%
  mutate(across(!c(ID, prec), ~map_dbl(., ~mean(.x$value)))) -> climate_data

return(climate_data)
}
