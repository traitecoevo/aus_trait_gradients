#require functions to load solar data for each path (load_solar_data)
source("R/load_climate_data_AWAP.R")

#############Start functions #################

######SOLAR######################

compile_daily_solar_data <- function(year){

if(sum(grepl(as.character(year), list.files("data/AWAP/AWAP_solar/compiled_annual_solar/")))>0){
print(paste0(year, " has already been done, moving onto next year"))
}

else{  
list.files("data/AWAP/AWAP_solar") %>%
  str_sub(1,4) %>%
  grep(as.character(year),.)->year_paths

list.files("data/AWAP/AWAP_solar")[year_paths] -> year_paths

year_paths%>%
  grep(".flt",.) -> year_paths_file

year_paths[year_paths_file] -> year_paths_file

solar_data<-purrr::map(year_paths_file, .f=load_solar_data)

solar_data %>%
  bind_rows()%>%
  mutate(key = stringr::str_replace_all(key, pattern="X", replacement=""))%>%
  mutate(key = stringr::str_replace_all(key, pattern="_rad", replacement=""))%>%
  separate(key, c("year","month","day"),c(4,6)) %>%
  rename(shortwave_radiation = value) -> solar_data

directory_output <- paste0("data/AWAP/AWAP_solar/compiled_annual_solar/solar_data_sites_",year,".csv")

write_csv(solar_data, directory_output)
}
}

read_and_combine_compiled_solar_data <- function(){
  paths <- list.files("data/AWAP/AWAP_solar/compiled_annual_solar/")
  paths_csv <- grep("compiled_total_solar_data.csv", paths)
  
  if(length(paths_csv) > 0){
    print("Data already compiled, can now read csv below")
  }
  
  if (length(paths_csv) == 0){
  yearly_paths<-paste("data/AWAP/AWAP_solar/compiled_annual_solar/",list.files("data/AWAP/AWAP_solar/compiled_annual_solar/"), sep="")
  purrr::map(yearly_paths, read_csv) %>%
    bind_rows() %>%
    write_csv("data/AWAP/AWAP_solar/compiled_annual_solar/compiled_total_solar_data.csv")
}
}

##########Rainfall####################

compile_annual_rainfall_data <- function(){
  test <- character(0)
  if(!identical(list.files("data/AWAP/agcd/v2/r005/01month/compiled_rainfall_data"), test)){
    stop("Compiled annual rainfall data already exists, no need to run again")
  }
  
  else{  
    list.files("data/AWAP/agcd/v2/r005/01month/") %>%
      str_sub(-3,-1) %>%
      grep(as.character(".nc"),.)->year_paths
    
    list.files("data/AWAP/agcd/v2/r005/01month/")[year_paths] -> year_paths_file
    
    rainfall_data <-purrr::map(year_paths_file, .f=load_rainfal_data)
    
    rainfall_data %>%
      bind_rows()%>%
      mutate(key = stringr::str_replace_all(key, pattern="X", replacement="")) %>%
      separate(key, c("year","month","day"),"[.]") %>%
      rename(prec = value) -> rainfall_data
    # 
    directory_output <- paste0("data/AWAP/agcd/v2/r005/01month/compiled_rainfall_data/compiled_rainfall_data.csv")
    
    write_csv(rainfall_data, directory_output)
  }
}

#################VP09#######################

compile_vp_09_data <- function(){
  test <- character(0)
  if(!identical(list.files("data/AWAP/agcd/v1/vapourpres_h09_monthly/mean/r005/01month/compiled_h09_data"), test)){
    stop("Compiled vp_09 data already exists, no need to run again")
  }
  
  else{  
    list.files("data/AWAP/agcd/v1/vapourpres_h09_monthly/mean/r005/01month/") %>%
      str_sub(-3,-1) %>%
      grep(as.character(".nc"),.)->year_paths
    
    list.files("data/AWAP/agcd/v1/vapourpres_h09_monthly/mean/r005/01month/")[year_paths] -> year_paths_file
    
    vp09_data <-purrr::map(year_paths_file, .f=load_vp09_data)
    
    vp09_data %>%
      bind_rows()%>%
      mutate(key = stringr::str_replace_all(key, pattern="X", replacement="")) %>%
      separate(key, c("year","month","day"),"[.]") %>%
      rename(vp09 = value) -> vp09_data

    directory_output <- paste0("data/AWAP/agcd/v1/vapourpres_h09_monthly/mean/r005/01month/compiled_h09_data/compiled_h09_data.csv")
    
    write_csv(vp09_data, directory_output)
  }
}

#################VP15#######################

compile_vp_15_data <- function(){
  test <- character(0)
  if(!identical(list.files("data/AWAP/agcd/v1/vapourpres_h15_monthly/compiled_h15_data/"), test)){
    stop("Compiled vp_15 data already exists, no need to run again")
  }
  
  else{  
    list.files("data/AWAP/agcd/v1/vapourpres_h15_monthly/") %>%
      str_sub(-3,-1) %>%
      grep(as.character(".nc"),.)->year_paths
    
    list.files("data/AWAP/agcd/v1/vapourpres_h15_monthly/")[year_paths] -> year_paths_file
    
    vp15_data <-purrr::map(year_paths_file, .f=load_vp15_data)
    
    vp15_data %>%
      bind_rows()%>%
      mutate(key = stringr::str_replace_all(key, pattern="X", replacement="")) %>%
      separate(key, c("year","month","day"),"[.]") %>%
      rename(vp15 = value) -> vp15_data
    
    directory_output <- paste0("data/AWAP/agcd/v1/vapourpres_h15_monthly/compiled_h15_data/compiled_h15_data.csv")
    
    write_csv(vp15_data, directory_output)
  }
}


#################max_T#######################

compile_max_T_data <- function(){
  test <- character(0)
  if(!identical(list.files("data/AWAP/agcd/v1/monthly_tmax/compiled_temperature_data/"), test)){
    stop("Compiled max_T data already exists, no need to run again")
  }
  
  else{  
    list.files("data/AWAP/agcd/v1/monthly_tmax/") %>%
      str_sub(-3,-1) %>%
      grep(as.character(".nc"),.)->year_paths
    
    list.files("data/AWAP/agcd/v1/monthly_tmax/")[year_paths] -> year_paths_file
    
    max_T_data <-purrr::map(year_paths_file, .f=load_max_t_data)
    
    max_T_data %>%
      bind_rows()%>%
      mutate(key = stringr::str_replace_all(key, pattern="X", replacement="")) %>%
      separate(key, c("year","month","day"),"[.]") %>%
      rename(max_t = value) -> max_T_data
    
    directory_output <- paste0("data/AWAP/agcd/v1/monthly_tmax/compiled_temperature_data/compiled_temperature_data.csv")
    
    write_csv(max_T_data, directory_output)
  }
}