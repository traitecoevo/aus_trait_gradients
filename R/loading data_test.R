library("tidyverse")

load_climate_data <- function(trait_data, simple = FALSE){
  
same <- TRUE
if(file.exists("data/climate_data.RDS")){
  climate_data <- readRDS("data/climate_data.RDS")
  same <- length(climate_data$ID %>% unique()) == length(trait_data$ID %>% unique())
  if(isFALSE(same)){
    warning("saved climate ids do not match trait ids, repeating the climate extraction process")
  }
}  
if(!file.exists("data/climate_data.RDS") | isFALSE(same))  
  {
trait_data %>%
    group_by(longitude, latitude,ID) %>%
    nest() %>%
    ungroup() %>%
    dplyr::select(-data)-> trait_data_coords
  
  
folders <- c("WC","envirem","wc2.1_30s_bio","VPD_Chelsa")  

extract_directory <- function(folders){
  list.files(paste0("data/climate_data/", folders), recursive = TRUE, full.names = TRUE) %>%
    terra::rast() -> stacked_rasters
  target_crs <- terra::crs(stacked_rasters)  
  australia <- terra::vect("data/AUS_2021_AUST_SHP_GDA2020/", crs=target_crs)
  
  stacked_rasters %>%
    terra::crop(australia) -> stacked_rasters_cropped
  
  stacked_rasters_cropped
}
extracted <- map(folders, extract_directory)
extracted[[4]] <- terra::resample(extracted[[4]],extracted[[1]][[1]], method = "bilinear")


extracted_stacked <- c(extracted[[1]], extracted[[2]], extracted[[3]], extracted[[4]])

if(simple == TRUE){
occurrence_data_species_family_vect <- terra::vect(trait_data_coords, 
                                                   geom = c("longitude","latitude"), 
                                                   crs = terra::crs(extracted_stacked))

terra::extract(x = extracted_stacked,  y = occurrence_data_species_family_vect,method = "simple", cells = TRUE, bind = TRUE, ID = FALSE) -> extracted_simple

extracted_simple <- terra::values(extracted_simple)
extracted_simple

extracted_simple %>%
  rename(wc2.1_30s_prec.wq = wc2.1_30s_bio_16,
         wc2.1_30s_prec.dq = wc2.1_30s_bio_17,
         wc2.1_30s_prec.cv = wc2.1_30s_bio_15,
         wc2.1_30s_temp.cv = wc2.1_30s_bio_4) %>%
  rename_with(~str_remove(string = ., pattern = "wc2.1_30s_"), .cols = starts_with("wc")) %>%
  rename_with(~str_remove(string = ., pattern = "current_30arcsec_"), .cols = starts_with("current_30arcsec_")) %>% 
  rename_with(~str_remove(string = ., pattern = "CHELSA_"), .cols = starts_with("CHELSA_")) %>% 
  rename_with(~str_remove(string = ., pattern = "_1981-2010_V.2.1"), .cols = ends_with("_1981-2010_V.2.1")) %>% 
  pivot_longer(-c(ID, cell)) %>%
  separate(name, into = c("env_name", "month"), sep="_(?=[^_]+$)") %>%
  group_by(ID, cell, env_name) %>%
  nest() -> data

data %>%
  pivot_wider(names_from = env_name, values_from = data) %>%
  ungroup() %>%
  mutate(prec = map_dbl(prec, ~.x %>% unnest() %>% summarise(value = sum(value)) %>% pull(value))) %>%
  mutate(across(!c(ID, prec, cell), ~map_dbl(., ~.x %>% unnest() %>% summarise(value = mean(value)) %>% pull(value))))-> climate_data


saveRDS(climate_data, "data/climate_data_simple.RDS")
}

extract_values <- function(start, end){
  extracted_stacked[[start:end]] %>%
    raster::stack() -> stacked_rasters_cropped_raster
  
  nearest_Land_transformed_coords<- seegSDM::nearestLand(trait_data_coords %>% dplyr::select(longitude, latitude), stacked_rasters_cropped_raster[[1]], max_distance  = 10000) %>% 
    as_tibble() %>%
    mutate(ID = trait_data_coords$ID)
  
  occurrence_data_species_family_vect <- terra::vect(nearest_Land_transformed_coords, 
                                                       geom = c("x","y"), 
                                                       crs = terra::crs(extracted_stacked))
  if(start == "1"){
  terra::extract(x = extracted_stacked[[start:end]],  y = occurrence_data_species_family_vect,method = "simple", cells = TRUE, bind = TRUE, ID = FALSE) -> extracted
  } else {
    terra::extract(x = extracted_stacked[[start:end]],  y = occurrence_data_species_family_vect,method = "simple", cells = FALSE, bind = TRUE, ID = FALSE) -> extracted
  }
  extracted <- terra::values(extracted)
  extracted
}


map_dbl(extracted, terra::nlyr) %>%
  tibble(nlyrs = .) %>% 
  mutate(start1 = purrr::accumulate(nlyrs, sum) - nlyrs +1) %>%
  mutate(end1 = start1 + nlyrs - 1) %>%
  map2(.x = .$start1, .y = .$end1, .f = ~extract_values(.x, .y)) %>%
  purrr::reduce(left_join) -> extracted

extracted %>%
  rename(wc2.1_30s_prec.wq = wc2.1_30s_bio_16,
         wc2.1_30s_prec.dq = wc2.1_30s_bio_17,
         wc2.1_30s_prec.cv = wc2.1_30s_bio_15,
         wc2.1_30s_temp.cv = wc2.1_30s_bio_4) %>%
  rename_with(~str_remove(string = ., pattern = "wc2.1_30s_"), .cols = starts_with("wc")) %>%
  rename_with(~str_remove(string = ., pattern = "current_30arcsec_"), .cols = starts_with("current_30arcsec_")) %>% 
  rename_with(~str_remove(string = ., pattern = "CHELSA_"), .cols = starts_with("CHELSA_")) %>% 
  rename_with(~str_remove(string = ., pattern = "_1981-2010_V.2.1"), .cols = ends_with("_1981-2010_V.2.1")) %>% 
  pivot_longer(-c(ID, cell)) %>%
  separate(name, into = c("env_name", "month"), sep="_(?=[^_]+$)") %>%
  group_by(ID, cell, env_name) %>%
  nest() -> data

data %>%
  pivot_wider(names_from = env_name, values_from = data) %>%
  ungroup() %>%
  mutate(prec = map_dbl(prec, ~.x %>% unnest() %>% summarise(value = sum(value)) %>% pull(value))) %>%
  mutate(across(!c(ID, prec, cell), ~map_dbl(., ~.x %>% unnest() %>% summarise(value = mean(value)) %>% pull(value))))-> climate_data

saveRDS(climate_data, "data/climate_data.RDS")
}
climate_data <- readRDS("data/climate_data.RDS")
return(climate_data)
}
