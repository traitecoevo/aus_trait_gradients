compile_terraclim_data_vpd <- function(){
  test <- character(0)
  if(!identical(list.files("data/TerraClimate_vpd/compiled_TerraClimate_vpd"), test)){
    stop("Compiled vpd_terraclimate data already exists, no need to run again")
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
      rename(vpd_terraclim = value) -> TerraClimate_vpd_data
    
    directory_output <- paste0("data/TerraClimate_vpd/compiled_TerraClimate_vpd/compiled_TerraClimate_vpd.csv")
    
    write_csv(TerraClimate_vpd_data, directory_output)
  }
}


