library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gWidgets)
options(guiToolkit="RGtk2")
library(readxl)
library(leaflet)


#WQS <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','wqs_riverine_id305b_2013_albers')
#select <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','userselecttest20132014_prj84') # try to break the userselection section with mutliple sites that need help in 1 file
#WQS_p <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_Planarized84')
#sites <- read.csv('data/sites_2009prob_SLIM.csv')

shinyServer(function(input,output,session){
    output$table <- renderTable({
      inFile <- input$sites
      if(is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)
    })
    finishText <- eventReactive(input$runButton,{withProgress(message='Making Connections',value=0,{
      # Bring in standards GIS layer, NWBD layer
      WQS <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','wqs_riverine_id305b_2013_albers')
      NWBD <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','vaNWBD_v4_albers')
      # Make shapefile from site csv file, copy projection from WQS layer
      inFile <- input$sites
      sites_shp <- read.csv(inFile$datapath)
      
      #sites_shp <- sites
      coordinates(sites_shp) <- ~Longitude+Latitude
      sites_shp@proj4string <- CRS("+proj=longlat +datum=WGS84") #first need to give it it's own projection 
      sites_shp <- spTransform(sites_shp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                             +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")) #then can transform it to same as other shapefiles
      #sites_shp@proj4string <- NWBD@proj4string
      # Make new df to store results
      output <- data.frame(matrix(NA, ncol = 18, nrow = 0))
      names(output) <- c(names(WQS@data),"VAHU6","SiteID","StationID","Comment")
      # Loop through new sites shapefile, buffering to latch on to WQS stream geometry
      for(i in 1:length(sites_shp)){
        print(i)
        site <- sites_shp[i,]
        #nwbd_site <- NWBD[site,]
        step1 <- gBuffer(sites_shp[i,],width=10)
        step1.2 <-WQS[step1,]
        if(nrow(step1.2@data)>0){
          #step1.2@data$VAHU6 <- nwbd_site@data[,3]
          step1.2@data$SiteID <- i
          step1.2@data$StationID <- sites_shp@data$StationID[i]
          step1.2@data$Comment <- ' '
          output <- rbind(output,step1.2@data)
        }else{
          step2 <- gBuffer(sites_shp[i,],width=20)
          step2.2 <- WQS[step2,]
          if(nrow(step2.2@data)>0){
            #step2.2@data$VAHU6 <- nwbd_site@data[,3]
            step2.2@data$SiteID <- i
            step2.2@data$StationID <- sites_shp@data$StationID[i]
            step2.2@data$Comment <- ' '
            output <- rbind(output,step2.2@data)
          }else{
            step3 <- gBuffer(sites_shp[i,],width=30)
            step3.2 <- WQS[step3,]
            if(nrow(step3.2@data)>0){
              #step3.2@data$VAHU6 <- nwbd_site@data[,3]
              step3.2@data$SiteID <- i
              step3.2@data$StationID <- sites_shp@data$StationID[i]
              step3.2@data$Comment <- ' '
              output <- rbind(output,step3.2@data)
            }else{
              step4 <- gBuffer(sites_shp[i,],width=40)
              step4.2 <- WQS[step4,]
              if(nrow(step4.2@data)>0){
                #step4.2@data$VAHU6 <- nwbd_site@data[,3]
                step4.2@data$SiteID <- i
                step4.2@data$StationID <- sites_shp@data$StationID[i]
                step4.2@data$Comment <- ' '
                output <- rbind(output,step4.2@data)
              }else{
                step5 <- gBuffer(sites_shp[i,],width=50)
                tryCatch({
                  step5.2 <- WQS[step5,]
                  if(nrow(step5.2@data)>0){
                    #step5.2@data$VAHU6 <- nwbd_site@data[,3]
                    step5.2@data$SiteID <- i
                    step5.2@data$StationID <- sites_shp@data$StationID[i]
                    step5.2@data$Comment <- ' '
                    output <- rbind(output,step5.2@data)
                  }else{
                    message <- data.frame(matrix(0,ncol=18,nrow=1))
                    names(message) <- names(output)
                    #message$VAHU6 <- nwbd_site@data[,3]
                    message$SiteID <- i
                    message$StationID <- sites_shp@data$StationID[i]
                    message$Comment <- 'use GIS for this site'
                    output <- rbind(output,message)}
                })
              }
            }
          }
        }
      }
      return(output)
    })
    })
    output$outputTable <- renderTable({finishText()})
})



#CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#writeOGR(sites_shp,"C:/HardDriveBackup/R/AssessmentTool/data","sites_shp2",driver="ESRI Shapefile")






#output$map305B <- renderLeaflet({
#  leaflet() %>%
#    addProviderTiles('Thunderforest.Outdoors') %>%
#    setView(lat=37.5,lng= -78.5,zoom=6)
#})



#leaflet(Virginia) %>% addProviderTiles('Thunderforest.Outdoors')

#observe({
#inFile <- input$sites
#if(!is.null(inFile())){
#  leafletProxy('map305B') %>%
# addMarkers(data=sites, lng=~Longitude,lat=~Latitude)
# }
#})