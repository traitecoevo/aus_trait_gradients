
load_climate_data <- function() {
  ## Austraits: Precipitation–temperature space plot
  #### 01 Get climate data for Australia ####
  # Download bioclim data using library (raster)
  bioclim <- raster::getData("worldclim", var = "bio", res = 10)
  # Pick BIO1 (Mean Annual Temperature; T), BIO12 (Annual Precipitation; P) and BIO15 (Prec Seasonality (CV))
  bioclim <- bioclim[[c(1, 12, 15)]]
  names(bioclim) <- c("Temp", "Prec", "Prec_CV")

  #### 02 Get the climate data for Australia ####
  # Load Australia landmass binary map
  au_map <- raster::raster("data/australia.tif")

<<<<<<< Updated upstream
  # Clip bioclim data with the au map
  ## crop and mask
  bioclim_au <- raster::crop(bioclim, raster::extent(au_map))

  new.bioclim <-
    raster::projectRaster(bioclim_au, au_map) # harmonize the spatial extent and projection

  au_bioclim <- raster::mask(new.bioclim, au_map)

=======
  # 
  # # Clip bioclim data with the au map
  # ## crop and mask
  # bioclim_au <- raster::crop(bioclim, raster::extent(au_map))
  # 
  new.bioclim <-
  raster::projectRaster(bioclim, au_map) # harmonize the spatial extent and projection

  solar<-raster::raster("data/Australia_GISdata_LTAym_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/Australia_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/DNI.tif")
  
  new.solar <-
    raster::projectRaster(solar, au_map) # harmonize the spatial extent and projection
  
  env.variables <- raster::addLayer(new.bioclim, new.solar)


  # au_bioclim <- terra::mask(new.bioclim, au_map)
  # au_bioclim <- terra::mask(bioclim_au, au_map)
  
  # 
>>>>>>> Stashed changes
  # # Transform raster data into a tibble
  # au_bioclim_table <- 
  #   au_bioclim %>%
  #   raster::as.data.frame() %>%
  #   na.omit() %>%
  #   as_tibble() %>%
  #   mutate(region = as.factor("Australia"))
  # au_bioclim_table
  env.variables
}



<<<<<<< Updated upstream
##### Functions ####
extract_climate_data <- function(df, climstack) {
  # df is a dataframe of trait data with lat and lon columns
  # climstack is a stack of gridded climate data

  df %>%
    ungroup() %>%
    dplyr::select(lat, lon) %>%
    na.omit() -> coord

  sp::coordinates(coord) <- ~ lon + lat
  sp::proj4string(coord) <- sp::CRS("+proj=longlat +datum=WGS84")
  raster::extract(climstack, coord, na.rm = F, df = T) -> df_climate

  df_climate
}


#' Combine species occurence and climate data
#'
#' @param species_occurence_df
#' @param climate_raster_data
#'
#' @return
#' @export
#'
#' @examples
=======
extract_climate_data_terra <- function(df, climstack) {
  # df is a dataframe of trait data with lat and lon columns
  # climstack is a stack of gridded climate data
  df_processed <- df %>%
    na.omit()
  
  df_processed %>% 
    mutate(clim = purrr::map2(lon, lat , ~ terra_extract(climstack, lon = .x, lat = .y))) %>% unnest(clim) %>% select(!paste0("V",as.character(seq(1:ncol(raster::values(au_bioclim))))))
  
}

terra_extract <- function(env_var, lon, lat){
  terra::extract(x = env_var,  y = sp::SpatialPoints(cbind(lon, lat)),method = "bilinear") %>% as_tibble()
}

>>>>>>> Stashed changes
combine_occurence_climate <- function(species_occurence_df, climate_raster_data) {
  species_occurence_df %>%
    group_by(ID) %>%
    rename(lat = latitude, lon = longitude) %>%
<<<<<<< Updated upstream
    extract_climate_data(climate_raster_data) %>%
    left_join(species_occurence_df, ., by = "ID") -> sp_clim_combined

=======
    extract_climate_data_terra(climate_raster_data) %>%
    merge(LOOKUP, ., by = "ID")   -> sp_clim_combined
  
>>>>>>> Stashed changes
  sp_clim_combined
}
