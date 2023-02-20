#from Cornwell et al. 2018

a <- 0.0819
b <- 0.0983
c <- 7.7521

calc_c_air_1992 <- function(latitude){
  a*sin(latitude*pi/180)^2 + b*sin(latitude*pi/180) - c
}

calc_c_air <- function(year, latitude){
  calc_c_air_1992(latitude) + -0.0227 * (year - 1992)
}
