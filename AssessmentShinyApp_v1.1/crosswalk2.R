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

#read in all necessary data
sites <- read.csv('C:/HardDriveBackup/R/AssessmentTool/AssessmentShinyApp_v1.1/data/sites_2009prob_SLIM.csv')
WQS <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','wqs_riverine_id305b_2013_albers')
NWBD <- readOGR('C:/HardDriveBackup/R/AssessmentTool/data','vaNWBD_v4_albers')
WQS_p <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_Planarized84')

sites_shp <- sites
coordinates(sites_shp) <- ~Longitude+Latitude
sites_shp@proj4string <- CRS("+proj=longlat +datum=WGS84") #first need to give it it's own projection 
sites_shp <- spTransform(sites_shp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                           +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")) #then can transform it to same as other shapefiles


output1 <- data.frame(matrix(NA, ncol = 18, nrow = 0))# Make new df to store results
names(output1) <- c(names(WQS@data),"VAHU6","SiteID","StationID","Comment")
for(i in 1:length(sites_shp)){
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

#translations____________________________________________________________
inputFile() == sites <- read.csv('data/sites_2009prob_SLIM.csv')
initialResults() == slimoutput
tableIssues() == tableIssues_tbl
tableIssues_shptbl() ==problemsites_tbl
##_______________________________________________________________________



tableIssues_tbl <- subset(slimoutput, slimoutput$Comment %in% c("Site Attached to 1+ Stream Geometry <50 m Buffer; See Advanced Mapping Tab","Use GIS for this site","Site Buffered 300 m; Review Results in Advanced Mapping Tab"))

problemsites_tbl <- subset(sites,StationID %in% unique(tableIssues_tbl$StationID))
problemsites <- subset(sites,StationID %in% unique(tableIssues_tbl$StationID)) %>%
  join(tableIssues_tbl[,17:18],by='StationID') %>%
  unique()



geometries <- tableIssues_tbl$ID305B
num_geometries <- which(WQS_p@data$ID305B %in% geometries)
WQS_p_selection <- WQS_p[num_geometries,]

#subset good sites
tableGoodSites <- subset(slimoutput, !(slimoutput$Comment %in% c("Site Attached to 1+ Stream Geometry <50 m Buffer; See Advanced Mapping Tab","Use GIS for this site","Site Buffered 300 m; Review Results in Advanced Mapping Tab")))

#user selection point in app
subTable <- tableIssues_tbl[1,]

# merge the two and add regional office info
comboTable <- rbind(tableGoodSites,subTable) %>%
  mutate(REGION='BRRO-R') %>% #input$regionalOffice)%>%
  select(REGION,everything(),-Comment)


#station name selection for IR stations table tab
stationNames <- as.character(comboTable$StationID)

#select same ID305B as stationID identified?>>>>
as.character(comboTable[which(comboTable$StationID==stationNames[1]),2])
as.character(sites[which(sites$StationID==stationNames[1]),2])

## Make comboTable() output same as Tish's table
comboTable2 <- comboTable %>%
  join(sites,by='StationID')%>%
  mutate(ID305B_1=ID305B,ID305B_2=NA,ID305B_3=NA,DEPTH=NA,STATION_ID=StationID,REGION='BRRO-R'#input$regionalOffice
         ,STATION_TYPE_1=NA,STATION_TYPE_2=NA,STATION_TYPE_3=NA,STATION_LAT=Latitude
         ,STATION_LON=Longitude,WATERSHED_ID=NA,TEMP_VIO=NA,TEMP_SAMP=NA,TEMP_STAT=NA
         ,DO_VIO=NA,DO_SAMP=NA,DO_STAT=NA,PH_VIO=NA,PH_SAMP=NA,PH_STAT=NA
         ,ECOLI_VIO=NA,ECOLI_SAMP=NA,ECOLI_STAT=NA,ENTER_VIO=NA,ENTER_SAMP=NA,ENTER_STAT=NA
         ,WATER_MET_VIO=NA,WATER_MET_STAT=NA,WATER_TOX_VIO=NA,WATER_TOX_STAT=NA
         ,SED_MET_VIO=NA,SED_MET_STAT=NA,SED_TOX_VIO=NA,SED_TOX_STAT=NA
         ,FISH_MET_VIO=NA,FISH_MET_STAT=NA,FISH_TOX_VIO=NA,FISH_TOX_STAT=NA
         ,BENTHIC_STAT=NA,NUT_TP_VIO=NA,NUT_TP_SAMP=NA,NUT_TP_STAT=NA
         ,NUT_CHLA_VIO=NA,NUT_CHLA_SAMP=NA,NUT_CHLA_STAT=NA,COMMENTS=NA)%>% #Create a bunch of new fields that are blank
  select(ID305B_1,ID305B_2,ID305B_3,DEPTH,STATION_ID,REGION
         ,STATION_TYPE_1,STATION_TYPE_2,STATION_TYPE_3,STATION_LAT
         ,STATION_LON,WATERSHED_ID,VAHU6,everything(),-c(Year,Latitude,Longitude,Region,St_Name,ID305B,WATER_NAME,BASIN,WQS_ID,WQS_SECTIO,WQS_CLASS,WQS_TROUT,WQS_SPECIA,WQS_SECT_1,WQS_COMMEN,DGIF_T_E,SHAPE_Leng,Miles,Tier_III,VAHU6,SiteID,StationID))



