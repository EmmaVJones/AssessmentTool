library(shiny)
library(shinyBS)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(reshape2)
library(plyr)
library(dplyr)
library(ggmap)
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
  
  #plotInput <- function(){renderLeaflet({
  #leaflet() %>% addProviderTiles('Thunderforest.Outdoors')%>%
  #  addPolylines(data=testshape_prj,color='red', weight=3
  #              ,group=testshape_prj@data$ID305B,popup=paste('ID305B:',testshape_prj@data$ID305B))
  # })}
  
  finishResults <- eventReactive(input$runButton,{withProgress(message='Making Connections',value=0,{
    ## Step 1: Bring in standards GIS layer, NWBD layer
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
    
    ## Step 2: Loop through sites buffering 10, 20, 30, 40, and 50m to connect point to line (stream) 
    # geometry from WQS layer
    output1 <- data.frame(matrix(NA, ncol = 18, nrow = 0))# Make new df to store results
    names(output1) <- c(names(WQS@data),"VAHU6","SiteID","StationID","Comment")
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
        output1 <- rbind(output1,step1.2@data)
      }else{
        step2 <- gBuffer(sites_shp[i,],width=20)
        step2.2 <- WQS[step2,]
        if(nrow(step2.2@data)>0){
          #step2.2@data$VAHU6 <- nwbd_site@data[,3]
          step2.2@data$SiteID <- i
          step2.2@data$StationID <- sites_shp@data$StationID[i]
          step2.2@data$Comment <- ' '
          output1 <- rbind(output1,step2.2@data)
        }else{
          step3 <- gBuffer(sites_shp[i,],width=30)
          step3.2 <- WQS[step3,]
          if(nrow(step3.2@data)>0){
            #step3.2@data$VAHU6 <- nwbd_site@data[,3]
            step3.2@data$SiteID <- i
            step3.2@data$StationID <- sites_shp@data$StationID[i]
            step3.2@data$Comment <- ' '
            output1 <- rbind(output1,step3.2@data)
          }else{
            step4 <- gBuffer(sites_shp[i,],width=40)
            step4.2 <- WQS[step4,]
            if(nrow(step4.2@data)>0){
              #step4.2@data$VAHU6 <- nwbd_site@data[,3]
              step4.2@data$SiteID <- i
              step4.2@data$StationID <- sites_shp@data$StationID[i]
              step4.2@data$Comment <- ' '
              output1 <- rbind(output1,step4.2@data)
            }else{
              step5 <- gBuffer(sites_shp[i,],width=50)
              tryCatch({
                step5.2 <- WQS[step5,]
                if(nrow(step5.2@data)>0){
                  #step5.2@data$VAHU6 <- nwbd_site@data[,3]
                  step5.2@data$SiteID <- i
                  step5.2@data$StationID <- sites_shp@data$StationID[i]
                  step5.2@data$Comment <- ' '
                  output1 <- rbind(output1,step5.2@data)
                }else{
                  message <- data.frame(matrix(0,ncol=18,nrow=1))
                  names(message) <- names(output1)
                  #message$VAHU6 <- nwbd_site@data[,3]
                  message$SiteID <- i
                  message$StationID <- sites_shp@data$StationID[i]
                  message$Comment <- 'use GIS for this site'
                  output1 <- rbind(output1,message)}
              })
            }
          }
        }
      }
    }
    ## Step 3 : Test if there were multiple connections made to a single site 
    output2 <- output1 %>%
      group_by(StationID) %>%
      ddply(c('SiteID','StationID'),summarise,records=length(StationID)) 
    # Look through output2 results and subset StationID's that grabbed +1 geometry
    df <- data.frame(StationID=character(),SiteID=numeric())
    for(i in 1:nrow(output2)){
      if(output2$records[i]==1){
        print(paste(i,'secondgo',sep=''))
      }else{
        placehold <- data.frame(StationID=as.character(output2$StationID[i]),SiteID=i)
        df <- rbind(df,placehold)
        rm(placehold)}
    }
    # Join results to output dataframe to attach geometry information
    df1 <- join(df,output1,by=c('StationID','SiteID'))
    # The Money loop: Loop with logic to deal with multiple geometries if encountered in buffering process
    # This uses an automatic selection if a StationID has multiple geometries with the same WQS_ID
    #  (it chooses the first entry) and a user selection component if a StationID has multiple
    #  geometries with multiple WQS_ID's
    sitelist <- list()
    autoselectentry <- data.frame()
    userselectentry <- data.frame()
    
    for(i in 1:length(unique(df1$StationID))){
      if(length(df1$StationID)>1){ 
        # If 1+ geometries identified in buffer steps then need to make a list of df's with unique 
        # StationID's to work through  
        splitStationID <- split(df1, df1$StationID, drop=T) #list of dataframes based on unique StationID's
        if(length(splitStationID[[i]]$StationID)>1){# If 1+ geometry identified for each StationID
          # If WQS_ID identical then automatically choose the first in the list
          if(length(unique(splitStationID[[i]]$WQS_ID))==1){
            # Save entry to a new dataframe to later append back to main geometry output dataframe
            splitdfselect <- splitStationID[[i]] # automatically select first entry if WQS_ID are identical
            autoselectentry <- rbind(autoselectentry,splitdfselect[!duplicated(splitdfselect[,c('StationID','SiteID')]),])
            # Save StationID to final output list
            sitelist[i] <- as.character(unique(splitdfselect$StationID))
          }else{# Then WQS_ID is different for the same StationID so go to user selection on following tab
            splitdfselect_comment <- splitStationID[[i]] %>% # automatically select first entry if WQS_ID are identical
              mutate(Comment=c('See Advanced Mapping Tab'))
            userselectentry <- rbind(userselectentry,splitdfselect_comment)
            # Save StationID to final output list
            sitelist[i] <- as.character(unique(splitdfselect_comment$StationID))
            # what do i need to find these sites in the next pane???????
            
            
            
          }
        }
      }
      # Create a final geometry output1 with only 1 stream geometry per site
      slimoutput <- filter(output1,!(StationID %in% sitelist)) %>%
        rbind(autoselectentry) %>%
        rbind(userselectentry)
    }
    
    
    
    return(slimoutput)
  })
  })
  output$outputTable <- DT::renderDataTable({DT::datatable(finishResults(),options=list(lengthMenu=
                                                                           list(c(5,15,-1),c('5','15','All'))
                                                                         ,pageLength=5))})
                                                           #,options=list(paging = FALSE))})
                                                           #,options=list(pageLength=25))})
  output$downloadResults <- downloadHandler(filename=function(){paste('Results_',input$sites,sep='')},
                                            content=function(file){write.csv(finishResults(),file)})
})


# next step, use data table selection (should i automatically highlight sites that need work in first pane?)
# and loop through those on the second tab, that tab should have 'run' button and radiobuttons that update 
# to allow user to choose correct 305b
# once user selections are made, need to output 'final' merged table for them to see and give download button





# Only load WQS planarized layer if needed and if not loaded into environment already
#if("WQS_p" %in% ls()){print('Good To GO!')
#}else(
#WQS_p <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_Planarized84'))
## Select site that is issue
#site <- factor(unique(splitStationID[[i]]$StationID), levels=levels(output1$StationID))
#num <- which(sites_shp@data$StationID %in% site) # Find plotting order of StationID in question
## Select geometries that are in question
#geom <- splitStationID[[i]]$ID305B
#num2 <- which(WQS_p@data$ID305B %in% geom) # Link selected ID305B info to actual WQS geometry
#testshape <- WQS_p[num2,]
#testshape@data$ID305B <- droplevels(testshape@data$ID305B) #get rid of excess factor levels for palette options
# leaflet attempt
#l<-leaflet() %>% addProviderTiles('Thunderforest.Outdoors') 
#pal <- colorFactor(rainbow(length(levels(testshape$ID305B))),domain=NULL)
#output$map <- renderLeaflet({
#  l%>%addPolylines(data=testshape,color=~pal(ID305B), weight=3,group=testshape@data$ID305B
#                   ,popup=paste('ID305B:',testshape@data$ID305B))})



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