library("tidyverse")

load_climate_data <- function(trait_data, simple_only = FALSE){
same <- FALSE
if(file.exists("data/climate_data_new.RDS")){
  climate_data <- readRDS("data/climate_data_new.RDS")
  same <- length(climate_data$ID %>% unique()) == length(trait_data$ID %>% unique())
  if(isFALSE(same)){
    warning("saved climate ids do not match trait ids, repeating the climate extraction process")
  }
}  
if(!file.exists("data/climate_data_new.RDS") | isFALSE(same))  
  {
trait_data %>%
    group_by(longitude, latitude,ID) %>%
    nest() %>%
    ungroup() %>%
    dplyr::select(-data)-> trait_data_coords
  
  
folders <- c("wc2.1_30s_bio","envirem","VPD_Chelsa")  

target_raster <- terra::rast("data/climate_data/WC/wc2.1_30s_elev/wc2.1_30s_elev.tif")
target_crs <- terra::crs(target_raster)

australia <- terra::vect("data/AUS_2021_AUST_SHP_GDA2020/", crs=target_crs) 

target_raster_cropped <- target_raster %>%
  terra::crop(australia)

extract_directory <- function(folders){
  list.files(paste0("data/climate_data/", folders), recursive = TRUE, full.names = TRUE)%>%
    terra::rast() -> stacked_rasters
  
  stacked_rasters %>%
    terra::project(target_raster_cropped, threads = TRUE)-> stacked_rasters_cropped
  
  stacked_rasters_cropped
}

extracted <- map(folders, extract_directory)
extracted_stacked <- c(extracted[[1]], extracted[[2]], extracted[[3]])


extract_values <- function(start, end){
  extracted_stacked[[start:end]] %>%
    raster::stack()-> stacked_rasters_cropped_raster
  
  if(start == "1"){

    nearest_Land_transformed_coords<- seegSDM::nearestLand(trait_data_coords %>% dplyr::select(longitude, latitude) %>% as.data.frame() ,stacked_rasters_cropped_raster[[1]],  max_distance  = 10000) %>% 
      as_tibble() %>%
      mutate(ID = trait_data_coords$ID) %>%
      group_by(ID) %>%
      nest() %>%
      ungroup() %>%
      mutate(data = map2(.x = data, .y = ID, ~if_else(is.na(.x$x) & is.na(.x$y), 
                                                     trait_data_coords %>%
                                                       filter(ID == .y) %>%
                                                       dplyr::select(x = longitude, y = latitude), 
                                                     .x))) %>%
      unnest(data) %>%
      ungroup()
    
    
    
    
    occurrence_data_species_family_vect <- terra::vect(nearest_Land_transformed_coords, 
                                                       geom = c("x","y"), 
                                                       crs = terra::crs(extracted_stacked))
    
    terra::extract(x = extracted_stacked[[start:end]],  y = occurrence_data_species_family_vect,method = "simple", cells = TRUE, bind = TRUE, ID = FALSE, xy = TRUE) -> extracted
    extracted <- terra::values(extracted)
    cell_nums <<- extracted$cell
    ID <<- extracted$ID
    
    
    } else {

    terra::extract(x = extracted_stacked[[start:end]],  y = cell_nums) -> extracted
    extracted$cell <- cell_nums
    extracted$ID <- ID
  }
  extracted
}


map_dbl(extracted, terra::nlyr) %>%
  tibble(nlyrs = .) %>% 
  mutate(start1 = purrr::accumulate(nlyrs, sum) - nlyrs +1) %>%
  mutate(end1 = start1 + nlyrs - 1) %>%
  map2(.x = .$start1, .y = .$end1, .f = ~extract_values(.x, .y)) -> out

out %>%
purrr::reduce(left_join) -> extracted

browser()

extracted %>%
  rename(wc2.1_30s_prec = wc2.1_30s_bio_12,
         wc2.1_30s_temp = wc2.1_30s_bio_1,
         wc2.1_30s_prec.wq = wc2.1_30s_bio_16,
         wc2.1_30s_prec.dq = wc2.1_30s_bio_17,
         wc2.1_30s_prec.cv = wc2.1_30s_bio_15,
         wc2.1_30s_temp.cv = wc2.1_30s_bio_4) %>%
  rename_with(~str_remove(string = ., pattern = "wc2.1_30s_"), .cols = starts_with("wc")) %>%
  rename_with(~str_remove(string = ., pattern = "current_30arcsec_"), .cols = starts_with("current_30arcsec_")) %>% 
  rename_with(~str_remove(string = ., pattern = "CHELSA_"), .cols = starts_with("CHELSA_")) %>% 
  rename_with(~str_remove(string = ., pattern = "_1981-2010_V.2.1"), .cols = ends_with("_1981-2010_V.2.1")) %>%
  dplyr::select(!contains("bio")) %>%
  pivot_longer(-c(ID, cell)) %>%
  separate(name, into = c("env_name", "month"), sep="_(?=[^_]+$)") %>%
  group_by(ID, cell, env_name) %>%
  nest() -> data

data %>%
  pivot_wider(names_from = env_name, values_from = data) %>%
  ungroup() %>%
  mutate(across(!c(ID, cell), ~map_dbl(., ~.x %>% unnest() %>% summarise(value = mean(value, na.rm = T)) %>% pull(value))))-> climate_data

saveRDS(climate_data, "data/climate_data_new.RDS")
}
climate_data <- readRDS("data/climate_data_new.RDS")
return(climate_data)
}
