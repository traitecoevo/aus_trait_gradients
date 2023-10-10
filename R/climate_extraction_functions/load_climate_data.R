library("tidyverse")

load_climate_data <- function(trait_data, simple_only = FALSE){

#initialise switch as FALSE for whether climate_data.RDS matches trait data
file_matches <- FALSE
browser()

#test whether climate_data.RDS exists and, if it does, compare against trait data to see if climate extraction needs to be rerun
if(file.exists("data/climate_data.RDS")){
  #if climate_data.RDS exists, load it
  climate_data <- readRDS("data/climate_data.RDS")
  #test whether the climate_data.RDS matches the trait data, if it does set file_matches to TRUE. Skips the extraction process and loads the file at bottom.
  file_matches <- (climate_data$ID %in% trait_data$ID %>% sum() == length(trait_data$ID %>% unique()) & length(unique(climate_data$ID)) == length(unique(trait_data$ID)))
  
  #if file_matches is FALSE, warns the user, and begins the climate extraction process
  if(isFALSE(file_matches)){
    warning("saved climate ids do not match trait ids, repeating the climate extraction process")
  }
} 

#if either climate_data.RDS does not exist or does not match, begin the climate extraction process
if(!file.exists("data/climate_data.RDS") | isFALSE(file_matches))  
  {
trait_data %>%
    #group by longitude, latitude and ID. Group by ID required because sometimes lat and long are equivalent because of trait collections from different datasets at nearby sites
    group_by(longitude, latitude,ID) %>%
    nest() %>%
    ungroup() %>%
    dplyr::select(-data)-> trait_data_coords
  
#load climate datasets from three sources  
folders <- c("wc2.1_30s_bio","envirem","VPD_Chelsa")  
browser()
#load a target raster. We do this to set the baseline CRS and resolution (all climate layers are in the same res so no up or down-scaling required)
target_raster <- terra::rast("data/climate_data/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
#set the target crs
target_crs <- terra::crs(target_raster)

#load australia as a vector file to bound observations, using the target crs from above
australia <- terra::vect("data/AUS_2021_AUST_SHP_GDA2020/", crs=target_crs) 

#crop the extent of the target raster with the vector file above
target_raster_cropped <- target_raster %>%
  terra::crop(australia)

#directory-based extraction using recursive folder searching
extract_directory <- function(folders){
  list.files(paste0("data/climate_data/", folders), recursive = TRUE, full.names = TRUE)%>%
    terra::rast() -> stacked_rasters
  
  stacked_rasters %>%
    terra::project(target_raster_cropped, threads = TRUE)-> stacked_rasters_cropped
  
  stacked_rasters_cropped
}

#extract rasters from each folder and collate
extracted <- map(folders, extract_directory)
extracted_stacked <- c(extracted[[1]], extracted[[2]], extracted[[3]])

#this is the main engine of this function, extracting climate data based on locations from rasters
extract_values <- function(start, end){
  #select appropriate raster layers for a given climate product
  extracted_stacked[[start:end]] %>%
    #use raster here rather than terra, necessary for use with seegSDM::nearestLand
    raster::stack()-> stacked_rasters_cropped_raster
  
  #for the first layer, we use seegSDM::nearestLand to move  
  if(start == "1"){

    #nearestLand outputs a matrix a coordinates of same length as trait_data_coords, with new coords for coords that originally extracted for NA cells. They have a buffer of 10km with which to be moved
    #to a nearby cell. We define these new set of coordinates for a single layer, then use the coordinates for all other layers, so that we make sure that we are comparing like for like climate wise (i.e. instead of doing nearest land for each layer). This also saves computing time.
    #we start with worldclim because this is where precipitation is stored (the key variable)
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
    
    
    
    #with the new set of coordinates we can make a vector file with terra
    occurrence_data_species_family_vect <- terra::vect(nearest_Land_transformed_coords, 
                                                       geom = c("x","y"), 
                                                       crs = terra::crs(extracted_stacked))
    #then extracted climate values for worldclim (i.e. the first folder)
    terra::extract(x = extracted_stacked[[start:end]],  y = occurrence_data_species_family_vect,method = "simple", cells = TRUE, bind = TRUE, ID = FALSE, xy = TRUE) -> extracted
    #turn extracted values to a dataframe
    extracted <- terra::values(extracted)
    
    #assign cells_nums amd ID to the global environment, these are passed to the next loop (the else statement)
    cell_nums <<- extracted$cell
    ID <<- extracted$ID
    
    
    } else {
    #extractions for the remaining folders
    terra::extract(x = extracted_stacked[[start:end]],  y = cell_nums) -> extracted
      
    #now assigned to extracted rather than to the global environment
    extracted$cell <- cell_nums
    extracted$ID <- ID
  }
  extracted
}

#identify the number of layers in each raster stack
map_dbl(extracted, terra::nlyr) %>%
  tibble(nlyrs = .) %>% 
  #identify the start and end layer number of each climate product
  mutate(start = purrr::accumulate(nlyrs, sum) - nlyrs +1) %>%
  mutate(end = start + nlyrs - 1) %>%
  map2(.x = .$start, .y = .$end, .f = ~extract_values(.x, .y)) -> out

out %>%
purrr::reduce(left_join) -> extracted

browser()

#find mean value of each climate layer in each cell
extracted %>%
  dplyr::select(wc2.1_30s_prec = wc2.1_30s_bio_12,
         wc2.1_30s_temp = wc2.1_30s_bio_1,
         wc2.1_30s_prec.wq = wc2.1_30s_bio_16,
         wc2.1_30s_prec.dq = wc2.1_30s_bio_17,
         wc2.1_30s_prec.cv = wc2.1_30s_bio_15,
         wc2.1_30s_temp.cv = wc2.1_30s_bio_4,
         wc2.1_30s_prec.hq = wc2.1_30s_bio_18,
         wc2.1_30s_prec.cq = wc2.1_30s_bio_19,
         starts_with("current_30arcsec_"),
         ends_with("_1981-2010_V.2.1"),
         cell,
         ID) %>%
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

saveRDS(climate_data, "data/climate_data.RDS")
}
climate_data <- readRDS("data/climate_data.RDS")
return(climate_data)
}
