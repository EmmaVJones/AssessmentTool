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
library(DT)


#WQS <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','wqs_riverine_id305b_2013_albers')
#select <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','userselecttest20132014_prj84') # try to break the userselection section with mutliple sites that need help in 1 file
#WQS_p <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_Planarized84')
#sites <- read.csv('C:/HardDriveBackup/R/AssessmentTool/data/sites_2009prob.csv')

shinyServer(function(input,output,session){
  #### BASIC TOOL TAB ####
  inputFile <- reactive({inFile <- input$sites
  if(is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath)
  })
  output$inputTable <- renderTable({inputFile()})
  
  initialResults <- eventReactive(input$runButton,{withProgress(message='Processing Sites',value=0,{
    incProgress(0.25, detail = 'Loading GIS data')
    ## Step 1: Bring in standards GIS layer, NWBD layer
    WQS <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','wqs_riverine_id305b_2013_albers')
    NWBD <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','vaNWBD_v4_albers')
    # Make shapefile from site csv file, copy projection from WQS layer
    sites_shp <- inputFile()
    
    #sites_shp <- sites
    coordinates(sites_shp) <- ~Longitude+Latitude
    sites_shp@proj4string <- CRS("+proj=longlat +datum=WGS84") #first need to give it it's own projection 
    sites_shp <- spTransform(sites_shp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                           +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")) #then can transform it to same as other shapefiles
    
    ## Step 2: Loop through sites buffering 10, 20, 30, 40, and 50m to connect point to line (stream) 
    # geometry from WQS layer
    output1 <- data.frame(matrix(NA, ncol = 18, nrow = 0))# Make new df to store results
    names(output1) <- c(names(WQS@data),"VAHU6","SiteID","StationID","Comment")
    withProgress(message = 'Making Connections', detail = "Site 1", value = 0, {
      # Loop through new sites shapefile, buffering to latch on to WQS stream geometry
      for(i in 1:length(sites_shp)){
        # Increment the progress bar, and update the detail text.
        incProgress(0.015, detail = paste("Site ", i))
        
        site <- sites_shp[i,]
        nwbd_site <- NWBD[site,]
        step1 <- gBuffer(sites_shp[i,],width=10)
        step1.2 <-WQS[step1,]
        if(nrow(step1.2@data)>0){
          step1.2@data$VAHU6 <- nwbd_site@data[,3]
          step1.2@data$SiteID <- i
          step1.2@data$StationID <- sites_shp@data$StationID[i]
          step1.2@data$Comment <- ' '
          output1 <- rbind(output1,step1.2@data)
        }else{
          step2 <- gBuffer(sites_shp[i,],width=20)
          step2.2 <- WQS[step2,]
          if(nrow(step2.2@data)>0){
            step2.2@data$VAHU6 <- nwbd_site@data[,3]
            step2.2@data$SiteID <- i
            step2.2@data$StationID <- sites_shp@data$StationID[i]
            step2.2@data$Comment <- ' '
            output1 <- rbind(output1,step2.2@data)
          }else{
            step3 <- gBuffer(sites_shp[i,],width=30)
            step3.2 <- WQS[step3,]
            if(nrow(step3.2@data)>0){
              step3.2@data$VAHU6 <- nwbd_site@data[,3]
              step3.2@data$SiteID <- i
              step3.2@data$StationID <- sites_shp@data$StationID[i]
              step3.2@data$Comment <- ' '
              output1 <- rbind(output1,step3.2@data)
            }else{
              step4 <- gBuffer(sites_shp[i,],width=40)
              step4.2 <- WQS[step4,]
              if(nrow(step4.2@data)>0){
                step4.2@data$VAHU6 <- nwbd_site@data[,3]
                step4.2@data$SiteID <- i
                step4.2@data$StationID <- sites_shp@data$StationID[i]
                step4.2@data$Comment <- ' '
                output1 <- rbind(output1,step4.2@data)
              }else{
                step5 <- gBuffer(sites_shp[i,],width=50)
                step5.2 <- WQS[step5,]
                if(nrow(step4.2@data)>0){
                  step5.2@data$VAHU6 <- nwbd_site@data[,3]
                  step5.2@data$SiteID <- i
                  step5.2@data$StationID <- sites_shp@data$StationID[i]
                  step5.2@data$Comment <- ' '
                  output1 <- rbind(output1,step5.2@data)
                }else{
                  step6 <- gBuffer(sites_shp[i,],width=300)
                  tryCatch({
                    step6.2 <- WQS[step6,]
                    if(nrow(step6.2@data)>0){
                      step6.2@data$VAHU6 <- nwbd_site@data[,3]
                      step6.2@data$SiteID <- i
                      step6.2@data$StationID <- sites_shp@data$StationID[i]
                      step6.2@data$Comment <- 'Site Buffered 300 m; Review Results in Advanced Mapping Tab'
                      output1 <- rbind(output1,step6.2@data)
                    }else{
                      message <- data.frame(matrix(0,ncol=18,nrow=1))
                      names(message) <- names(output1)
                      message$VAHU6 <- nwbd_site@data[,3]
                      message$SiteID <- i
                      message$StationID <- sites_shp@data$StationID[i]
                      message$Comment <- 'Use GIS for this site'
                      output1 <- rbind(output1,message)}
                  })}}}}}}
      })
    ## Step 3 : Test if there were multiple connections made to a single site 
    output2 <- output1 %>%
      group_by(StationID) %>%
      ddply(c('SiteID','StationID'),summarise,records=length(StationID)) 
    # Look through output2 results and subset StationID's that grabbed +1 geometry
    df <- data.frame(StationID=character(),SiteID=numeric())
    # Progress Bar Update, halfway done
    incProgress(0.5)
    
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
            if(splitStationID[[i]]$Comment[1]=="Site Buffered 300 m; Review Results in Advanced Mapping Tab"){
              splitdfselect_comment <- splitStationID[[i]] %>% # automatically select first entry if WQS_ID are identical
                mutate(Comment=c('Site Buffered 300 m; Review Results in Advanced Mapping Tab'))
              userselectentry <- rbind(userselectentry,splitdfselect_comment)
              # Save StationID to final output list
              sitelist[i] <- as.character(unique(splitdfselect_comment$StationID))
            }else{
            # Save entry to a new dataframe to later append back to main geometry output dataframe
            splitdfselect <- splitStationID[[i]] # automatically select first entry if WQS_ID are identical
            autoselectentry <- rbind(autoselectentry,splitdfselect[!duplicated(splitdfselect[,c('StationID','SiteID')]),])
            # Save StationID to final output list
            sitelist[i] <- as.character(unique(splitdfselect$StationID))}
          }else{# Then WQS_ID is different for the same StationID so go to user selection on following tab
            splitdfselect_comment <- splitStationID[[i]] %>% # automatically select first entry if WQS_ID are identical
              mutate(Comment=c('Site Attached to 1+ Stream Geometry <50 m Buffer; See Advanced Mapping Tab'))
            userselectentry <- rbind(userselectentry,splitdfselect_comment)
            # Save StationID to final output list
            sitelist[i] <- as.character(unique(splitdfselect_comment$StationID))
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
  
  output$resultsTable <- renderDataTable({datatable(initialResults(),options=list(lengthMenu=
                                                                                    list(c(5,15,-1),c('5','15','All'))
                                                                                  ,pageLength=5))})
  
  tableIssues <- reactive({subset(initialResults(), initialResults()$Comment %in% c("Site Attached to 1+ Stream Geometry <50 m Buffer; See Advanced Mapping Tab","Use GIS for this site","Site Buffered 300 m; Review Results in Advanced Mapping Tab"))})
  
  output$outputTableIssues <- renderDataTable({datatable(tableIssues()
                                                         ,options=list(lengthMenu=list(c(5,15,-1),c('5','15','All')),pageLength=5))})
  
  output$downloadResults <- downloadHandler(filename=function(){paste('Results_',input$sites,sep='')},
                                            content=function(file){write.csv(initialResults(),file)}) #will need to change to finalfinal finish results
  
  #### ADVANCED MAPPING TAB ####
  problemsites_tbl <- reactive({if(!is.null(inputFile())){
    problemsites <- subset(inputFile(),StationID %in% unique(tableIssues()$StationID)) %>%
      join(tableIssues()[,17:18],by='StationID') %>%
      unique()}})
  
  output$outputTableIssues_test <- renderTable({problemsites_tbl()})
  
  observe({if(input$runButton2==T){withProgress(message='Processing Sites',value=0,{
    incProgress(0.25, detail = 'Loading GIS data')
    # Bring in planarized WQS layer to plot on leaflet map, reg version won't work properly
    WQS_p <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_Planarized84')
    geometries <- tableIssues()$ID305B
    num_geometries <- which(WQS_p@data$ID305B %in% geometries)
    WQS_p_selection <- WQS_p[num_geometries,]
    WQS_p_selection@data$ID305B <- droplevels(WQS_p_selection@data$ID305B) #get rid of excess factor levels for palette options
    pal <- colorFactor(rainbow(7),domain=NULL)
    
    # Increment the progress bar
    for (i in 1:length(tableIssues())) {incProgress(0.015, detail = paste("Processing Site ", i))}
    
    output$issueMap <- renderLeaflet({
      leaflet() %>% addProviderTiles('Thunderforest.Outdoors') %>%
        setView(lat=37.342,lng=-79.740,zoom=6) %>%
        addMarkers(data=problemsites_tbl(),popup=paste(sep="<br/>",problemsites_tbl()$StationID
                                                       ,problemsites_tbl()$Comment)) %>%
        addPolylines(data=WQS_p_selection,color=~pal(ID305B), weight=5,group=WQS_p_selection@data$ID305B
                     ,popup=paste('ID305B:',WQS_p_selection@data$ID305B))})
  
    ##### ID305B User Selection Component 
    tableIssues()
    #output$optionsID305B <- renderUI({
    #  selectInput("optionsID305B_list","ID305B Selection",tableIssues())#[,1])
    #}) ### will this work once I identify list to show?
    
    
    })
  }}) 
})