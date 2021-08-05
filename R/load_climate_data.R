
load_climate_data <- function() {
  ## Austraits: Precipitation–temperature space plot
  #### 01 Get climate data for Australia ####
  # Download bioclim data using library (raster)
  bioclim <- raster::getData("worldclim", var = "bio", res = 2.5)
  # Pick BIO1 (Mean Annual Temperature; T), BIO12 (Annual Precipitation; P) and BIO15 (Prec Seasonality (CV))
  bioclim <- bioclim[[c(1, 12, 15)]]
  names(bioclim) <- c("Temp", "Prec", "Prec_CV")
  
  #### 02 Get the climate data for Australia ####
  # Load Australia landmass binary map
  au_map <- raster::raster("data/australia.tif")
  # 
  # # Clip bioclim data with the au map
  # ## crop and mask
  bioclim_au <- raster::crop(bioclim, raster::extent(au_map))
  # 
  new.bioclim <-
    raster::projectRaster(bioclim, au_map) # harmonize the spatial extent and projection
  # 
  # au_bioclim <- terra::mask(new.bioclim, au_map, touches=FALSE)
  # 
  # # Transform raster data into a tibble
  # au_bioclim_table <- 
  #   au_bioclim %>%
  #   raster::as.data.frame() %>%
  #   na.omit() %>%
  #   as_tibble() %>%
  #   mutate(region = as.factor("Australia"))
  # au_bioclim_table
  new.bioclim
}



extract_climate_data <- function(df, climstack) {
  # df is a dataframe of trait data with lat and lon columns
  # climstack is a stack of gridded climate data
  df_processed <- df %>%
    na.omit()
  
  df_processed %>% 
    mutate(clim = purrr::map2(lon, lat , ~ terra_extract(au_bioclim, lon = .x, lat = .y))) %>% unnest(clim)
  
}


terra_extract <- function(env_var, lon, lat){
  terra::extract(x = env_var,  y = sp::SpatialPoints(cbind(lon, lat)),method = "simple") %>% as_tibble()
}


combine_occurence_climate <- function(species_occurence_df, climate_raster_data) {
  LOOKUP <- species_occurence_df %>% select(ID)
  species_occurence_df %>%
    group_by(ID) %>%
    rename(lat = latitude, lon = longitude) %>%
    extract_climate_data(climate_raster_data) %>%
    merge(LOOKUP, ., by = "ID")   -> sp_clim_combined
  
  sp_clim_combined
}