packages <- c('shiny','shinyBS','shinyFiles','raster','rgdal','maptools','rgeos','reshape2','plyr','dplyr','ggmap','leaflet','devtools')

for (i in packages) {
  install.packages(i)
} 

library(devtools)

devtools::install_github('rstudio/DT') #install development version of DT package for cool data table selection stuff

