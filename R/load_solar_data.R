rm(list=ls())

load_solar_data <- function(year,month) {

au_map <- raster::raster("data/australia.tif")
  
  
for(i in 1:length(month)){
  if(i == 1){
    path<-paste0("data/PAR_eMAST/eMAST-R-Package_v1-1_photosynthetically-active-radiation_monthly_0-01deg_1970-2012_00000000_",year,month[i],".nc",sep="")
    asd <- raster::brick(path)
    asd[asd < 0] <- NA
    asd <-
      raster::projectRaster(asd, au_map) # harmonize the spatial extent and projection
    
  }
  if(i != 1){
    path<-paste0("data/PAR_eMAST/eMAST-R-Package_v1-1_photosynthetically-active-radiation_monthly_0-01deg_1970-2012_00000000_",year,month[i],".nc",sep="")
    new_layer <- raster::brick(path)
    new_layer[new_layer < 0] <- NA
    new_layer <-
      raster::projectRaster(new_layer, au_map) # harmonize the spatial extent and projection
    
    asd <- raster::addLayer(asd, new_layer) 
  }
}
 calc(asd, fun = mean)
}

# year <- as.character(1970)
# month <- as.character(c(paste0("0",seq(9)), seq(from=10, to=12)))
# 
# asdf <- load_solar_data(year,month)
# 
# pdf("output/Annual_monthly_average_PAR_1970.pdf",height = 4, width = 6)
# raster::plot(asdf, main = "Annual average of monthly averages PAR 1970.pdf")
# dev.off()
