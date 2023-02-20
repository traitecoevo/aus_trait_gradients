compile_vpd_terraclim_data <- function(){
  test <- character(0)
  if(!identical(list.files("data/TerraClimate_vpd/compiled_TerraClimate_vpd"), test)){
    stop("Compiled vp_09 data already exists, no need to run again")
  }
  
  else{  
    list.files("data/TerraClimate_vpd/") %>%
      str_sub(-3,-1) %>%
      grep(as.character(".nc"),.)->year_paths
    
    list.files("data/TerraClimate_vpd/")[year_paths] -> year_paths_file
    
    TerraClimate_vpd_data <-purrr::map(year_paths_file, .f=load_TerraClimate_vpd)
    
    TerraClimate_vpd_data %>%
      bind_rows()%>%
      mutate(key = stringr::str_replace_all(key, pattern="X", replacement="")) %>%
      separate(key, c("year","month","day"),"[.]") %>%
      rename(vp09 = value) -> TerraClimate_vpd_data
    
    directory_output <- paste0("data/TerraClimate_vpd/compiled_TerraClimate_vpd.csv")
    
    write_csv(TerraClimate_vpd_data, directory_output)
  }
}
  
  
load_TerraClimate_vpd <- function(path){
  TerraClimate_vpd_path <- paste0("data/TerraClimate_vpd/",as.character(path))
  TerraClimate_vpd<-raster::stack(TerraClimate_vpd_path)
  new_TerraClimate_vpd <-
    raster::projectRaster(TerraClimate_vpd, au_map) # harmonize the spatial extent and projection
  coordinates <- dplyr::select(location_of_sites, longitude, latitude)
  raster::extract(x = new_TerraClimate_vpd,  y = sp::SpatialPoints(coordinates, proj4string=raster::crs(au_map)),method = "simple") %>% 
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
    raster::extract(x = new_TerraClimate_vpd[[i]],  y = sp::SpatialPoints(extracted_object_NA_coordinates, proj4string=raster::crs(au_map)), buffer = 5000, fun = mean, na.rm=T) %>%
      as_tibble() %>%
      mutate(key = names(new_TerraClimate_vpd[[i]]), latitude = extracted_object_NA$latitude,longitude = extracted_object_NA$longitude, ID = extracted_object_NA$ID, buffer_extracted = "yes")
  }
  
  map(.x = seq(length(names(new_TerraClimate_vpd))), .f=iterate_buffer) %>%
    bind_rows() -> extracted_NA_values
  
  extracted_object%>%
    drop_na(value) %>%
    mutate(buffer_extracted = "no") %>%
    bind_rows(extracted_NA_values)
}

compile_vpd_terraclim_data()
