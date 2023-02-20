library("tidyverse")

load_climate_data <- function(trait_data){

same <- TRUE
  browser()
if(file.exists("data/new_climate_data.RDS")){
  climate_data <- readRDS("data/new_climate_data.RDS")
  same <- length(climate_data$ID %>% unique()) == length(trait_data$ID %>% unique())
  if(isFALSE(same)){
    warning("saved climate ids do not match trait ids, repeating the climate extraction process")
  }
}  
if(!file.exists("data/new_climate_data.RDS") | isFALSE(same))  
  {
australia <- raster::raster("data/australia.tif")
ext <- raster::extent(australia)

trait_data %>%
  group_by(longitude, latitude,ID) %>%
  nest() %>%
  ungroup() -> trait_data_coords

coords <-  trait_data_coords[, c('longitude','latitude')] 

list.files(paste("data/climate_data/WC/",list.files("data/climate_data/WC/"),sep = ""), full.names = TRUE)%>%
  terra::rast() %>%
  raster::crop(ext) %>%
  raster::stack() -> raster_WC
  
list.files(paste("data/climate_data/wc2.1_30s_bio/",sep = ""), full.names = TRUE)[c(7,8,9,14)] %>%
  terra::rast() %>%
  raster::crop(ext) %>%
  raster::stack() -> raster_bioclim

raster_WC <- raster::stack(raster_WC, raster_bioclim)

list.files(paste("data/climate_data/envirem/",sep = ""), full.names = TRUE)[c(1,2,3,7)] %>%
  terra::rast() %>%
  raster::crop(ext) %>%
  raster::stack()-> raster_envirem

browser()
list.files(paste("data/climate_data/VPD_Chelsa/",sep = ""), full.names = TRUE)%>%
  terra::rast() %>%
  raster::crop(ext)%>%
  raster::stack()-> raster_Chelsa
  
raster_WC %>%
  raster::stack(raster_envirem) %>%
  raster::stack(raster_Chelsa)-> raster_stack
browser()
lis <- list(raster_WC, raster_envirem, raster_Chelsa)
extracted_values <- list()

for(i in 1:length(lis)){

  if(i == 1){
    terra::extract(x = raster_stack[[1:raster::nlayers(raster_WC)]],  y = sp::SpatialPoints(coords = cbind(coords$longitude, coords$latitude), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>%
      as_tibble() %>%
      mutate(ID = trait_data_coords$ID, latitude = trait_data_coords$latitude, longitude = trait_data_coords$longitude)-> extracted_values_tmp
    NA_coords <- extracted_values_tmp %>%
      filter(if_any(!c(ID), ~(is.na(.))))

    new_coords <- seegSDM::nearestLand(NA_coords %>% dplyr::select(longitude,latitude), raster_WC[[1]], max_distance = 10000) %>%
      as_tibble() %>%
      mutate(ID = NA_coords$ID) %>%
      drop_na()
    terra::extract(x = raster_stack[[1:raster::nlayers(raster_WC)]],  y = sp::SpatialPoints(coords = cbind(new_coords$x, new_coords$y), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>%
      as_tibble() %>%
      mutate(ID = new_coords$ID, latitude = new_coords$y, longitude = new_coords$x)-> extracted_values_tmp_new

    extracted_values_tmp%>%
      filter(if_any(!c(ID, latitude, longitude,cells), ~(!is.na(.)))) %>%
      bind_rows(extracted_values_tmp_new) -> extracted_values[["WC"]]
      }
  if(i == 2){
    terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + 1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem))]],  y = sp::SpatialPoints(coords = cbind(coords$longitude, coords$latitude), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>%
      as_tibble() %>%
      mutate(ID = trait_data_coords$ID, latitude = trait_data_coords$latitude, longitude = trait_data_coords$longitude)-> extracted_values_tmp

    NA_coords <- extracted_values_tmp %>%
      filter(if_any(!c(ID), ~(is.na(.))))

    new_coords <- seegSDM::nearestLand(NA_coords %>% dplyr::select(longitude,latitude), raster_envirem[[1]], max_distance = 10000) %>%
      as_tibble() %>%
      mutate(ID = NA_coords$ID) %>%
      drop_na()

    terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + 1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem))]],  y = sp::SpatialPoints(coords = cbind(new_coords$x, new_coords$y), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>%
      as_tibble() %>%
      mutate(ID = new_coords$ID, latitude = new_coords$y, longitude = new_coords$x)-> extracted_values_tmp_new

    extracted_values_tmp%>%
      filter(if_any(!c(ID, latitude, longitude,cells), ~(!is.na(.)))) %>%
      bind_rows(extracted_values_tmp_new) -> extracted_values[["envirem"]]
  }

  if(i == 3){
    browser()
    terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+raster::nlayers(raster_Chelsa))]],  y = sp::SpatialPoints(coords = cbind(coords$longitude, coords$latitude), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>% 
      as_tibble() %>%
      mutate(ID = trait_data_coords$ID, latitude = trait_data_coords$latitude, longitude = trait_data_coords$longitude)-> extracted_values_tmp  
    
    NA_coords <- extracted_values_tmp %>%
      filter(if_any(!c(ID), ~(is.na(.))))
    
    if(nrow(NA_coords) > 0){
    new_coords <- seegSDM::nearestLand(NA_coords %>% dplyr::select(longitude,latitude), raster_envirem[[1]], max_distance = 10000) %>%
      as_tibble() %>%
      mutate(ID = NA_coords$ID) %>%
      drop_na()
    
    terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + 1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem))]],  y = sp::SpatialPoints(coords = cbind(new_coords$x, new_coords$y), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>%
      as_tibble() %>%
      mutate(ID = new_coords$ID, latitude = new_coords$y, longitude = new_coords$x)-> extracted_values_tmp_new
    
    extracted_values_tmp%>%
      filter(if_any(!c(ID, latitude, longitude,cells), ~(!is.na(.)))) %>%
      bind_rows(extracted_values_tmp_new) -> extracted_values[["Chelsa"]]
    } else{
      extracted_values_tmp-> extracted_values[["Chelsa"]]
    }
  }
}

browser()

extracted_values %>%
  purrr::reduce(left_join) %>%
  rename(wc2.1_30s_prec.wq = wc2.1_30s_bio_16,
         wc2.1_30s_prec.dq = wc2.1_30s_bio_17,
         wc2.1_30s_prec.cv = wc2.1_30s_bio_15,
         wc2.1_30s_temp.cv = wc2.1_30s_bio_4) %>%
  rename_with(~str_remove(string = ., pattern = "wc2.1_30s_"), .cols = starts_with("wc")) %>%
  rename_with(~str_remove(string = ., pattern = "current_30arcsec_"), .cols = starts_with("current_30arcsec_")) %>% 
  rename_with(~str_remove(string = ., pattern = "CHELSA_"), .cols = starts_with("CHELSA_")) %>% 
  rename_with(~str_remove(string = ., pattern = "_1981.2010_V.2.1"), .cols = ends_with("_1981.2010_V.2.1")) %>% 
  pivot_longer(-c(ID, cells)) %>%
  separate(name, into = c("env_name", "month"), sep="_(?=[^_]+$)") %>%
  group_by(ID, cells, env_name) %>%
  nest() -> data

data %>%
  pivot_wider(names_from = env_name, values_from = data) %>%
  ungroup() %>%
  mutate(prec = map_dbl(prec, ~.x %>% unnest() %>% summarise(value = sum(value)) %>% pull(value))) %>%
  mutate(across(!c(ID, prec, cells), ~map_dbl(., ~.x %>% unnest() %>% summarise(value = mean(value)) %>% pull(value))))-> climate_data

saveRDS(climate_data, "data/new_climate_data_chelsa.RDS")
}
climate_data <- readRDS("data/new_climate_data_chelsa.RDS")
return(climate_data)
}

























# f <- function(x, n){
#     output_name = paste0("data/processed_vpd_rasters/raster_part_", n, ".tif")
#   if(!file.exists(output_name)){
#     x %>%
#       raster::stack(., varname = "vpd") %>%
#       raster::projectRaster(crs = crs_envirem, res = res_envirem) %>%
#       raster::crop(y = ext) -> raster_terraclim
#     
#     num_layers = raster::nlayers(raster_terraclim)
#     
#     raster_terraclim <- raster::calc(raster_terraclim, fun = function(x) {sum(x)})-> raster_terraclim
# 
#     output_name = paste0("data/processed_vpd_rasters/raster_part_", n, ".tif")
#     raster::writeRaster(raster_terraclim, output_name)
#   }
#   return()
# }
# future::plan("multisession", workers = 6)
# browser()
# list.files(paste("data/climate_data/TerraClimate_vpd",sep = ""), full.names = TRUE) %>%
#   split(seq(1:30)) %>%
#   furrr::future_walk2(.x = ., .y = names(.),.f = f)
# 
# num_layers_vpd <- length(list.files(paste("data/climate_data/TerraClimate_vpd",sep = ""), full.names = TRUE))*12
# 
# raster_terraclim <- raster::stack(list.files("data/processed_vpd_rasters", full.names = T)) 
# raster_terraclim <- raster::calc(raster_terraclim, fun = function(x) {sum(x)/num_layers_vpd})
#   
# raster_WC %>%
#   raster::stack(raster_envirem) %>%
#   raster::stack(raster_terraclim)-> raster_stack
# 
# 
# 
# browser()
# terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+raster::nlayers(raster_terraclim))]],  y = sp::SpatialPoints(coords = cbind(coords$longitude, coords$latitude), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>% 
#   as_tibble() %>%
#   mutate(ID = trait_data_coords$ID, latitude = trait_data_coords$latitude, longitude = trait_data_coords$longitude)-> extracted_values_tmp  
# 
# NA_coords <- extracted_values_tmp %>%
#   filter(if_any(!c(ID), ~(is.na(.)))) 
# 
# new_coords <- seegSDM::nearestLand(NA_coords %>% select(longitude,latitude), raster_terraclim[[1]], max_distance = 10000) %>%
#   as_tibble() %>%
#   mutate(ID = NA_coords$ID) %>%
#   drop_na()
# if(nrow(new_coords) > 0){
#   terra::extract(x = raster_stack[[(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+1):(raster::nlayers(raster_WC) + raster::nlayers(raster_envirem)+raster::nlayers(raster_terraclim))]],  y = sp::SpatialPoints(coords = cbind(new_coords$V1, new_coords$V2), proj4string=raster::crs(raster_stack)),method = "simple", cellnum = TRUE) %>% 
#     as_tibble() %>%
#     mutate(ID = new_coords$ID, latitude = new_coords$V2, longitude = new_coords$V1)-> extracted_values_tmp_new 
#   extracted_values_tmp%>%
#     filter(if_any(!c(ID, latitude, longitude, cells), ~(!is.na(.)))) %>%
#     bind_rows(extracted_values_tmp_new) -> extracted_values[["terraclim"]]    
# } else{
#   extracted_values_tmp%>%
#     filter(if_any(!c(ID, latitude, longitude, cells), ~(!is.na(.))))-> extracted_values[["terraclim"]]    
# }
# }