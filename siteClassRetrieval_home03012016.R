## Run in R 3.2.3+

## This tool queries site lat/long on WQS layer to retrieve assessment standards



# 03/01/2016 update: Working user/automatic selection tool and logic to get there if too many
#                    geometries were grabbed in buffering step
# Long term goals: 1) Plot all streams with interactive basemap, conversion to geoJSON files in R
#                       - Should I be labeling anything else to help user differentiate correct geometry?
#                  2) Allow user to input excel file of sites instead of only shapefile
# I want to 1) Allow user to bring in data for site(s)- from Access? Excel?
#           2) run each selection against water quality standards rules
------------------------------------------------------------------------------------------------

## Load Libraries
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


## Step 1: Load GIS layers
# Could also use an excel file of lat/longs (with StationID,LONG_DD, and LAT_DD manditory column names)
# Will work on that component later
# Must have WQS and sites in same projection, if not alter @proj4string 
WQS <- readOGR('C:/GIS/EmmaGIS/Assessment','wqs_riverine_id305b_2013_albers')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2001_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2002_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2003_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2004_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2005_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2006_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2007_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2008_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2009_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2010_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2011_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2012_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2013_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2014_finalsites_EVJ')
#select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/ProcessedFiles','2015_finalsites_EVJ')
select <- readOGR('C:/GIS/ProbMonGIS/Rfiles/QA','userselecttest') # try to break the userselection section with mutliple sites that need help in 1 file

#select@data$StationID <- select@data$SiteID

## Step 2: Loop through sites buffering 10, 20, 30, 40, and 50m to connect point to line (stream) 
# geometry from WQS layer
output <- data.frame(matrix(NA, ncol = 17, nrow = 0))
names(output) <- c(names(WQS@data),"SiteID","StationID","Comment")
for(i in 1:length(select)){
  print(i)
  step1 <- gBuffer(select[i,],width=10)
  step1.2 <- try(WQS[step1,],silent=T)
  if(class(step1.2)!="try-error"){
    step1.2@data$SiteID <- i
    step1.2@data$StationID <- select@data$StationID[i]
    step1.2@data$Comment <- ' '
    output <- rbind(output,step1.2@data)
  }else{
    step2 <- gBuffer(select[i,],width=20)
    step2.2 <- try(WQS[step2,],silent=T)
    if(class(step2.2)!="try-error"){
      step2.2@data$SiteID <- i
      step2.2@data$StationID <- select@data$StationID[i]
      step2.2@data$Comment <- ' '
      output <- rbind(output,step2.2@data)
    }else{
      step3 <- gBuffer(select[i,],width=30)
      step3.2 <- try(WQS[step3,],silent=T)
      if(class(step3.2)!="try-error"){
        step3.2@data$SiteID <- i
        step3.2@data$StationID <- select@data$StationID[i]
        step3.2@data$Comment <- ' '
        output <- rbind(output,step3.2@data)
      }else{
        step4 <- gBuffer(select[i,],width=40)
        step4.2 <- try(WQS[step4,],silent=T)
        if(class(step4.2)!="try-error"){
          step4.2@data$SiteID <- i
          step4.2@data$StationID <- select@data$StationID[i]
          step4.2@data$Comment <- ' '
          output <- rbind(output,step4.2@data)
        }else{
          step5 <- gBuffer(select[i,],width=50)
          tryCatch({
            step5.2 <- try(WQS[step5,],silent=T)
            if(class(step5.2)!="try-error"){
              step5.2@data$SiteID <- i
              step5.2@data$StationID <- select@data$StationID[i]
              step5.2@data$Comment <- ' '
              output <- rbind(output,step5.2@data)
            }else{
              message <- data.frame(matrix(0,ncol=17,nrow=1))
              names(message) <- names(output)
              message$SiteID <- i
              message$StationID <- select@data$StationID[i]
              message$Comment <- 'use GIS for this site'
              output <- rbind(output,message)}
            })
        }
      }
    }
  }
}


## Step 3 : Test if there were multiple connections made to a single site 
#output2 <- output %>%
  #group_by(StationID) %>%
  #summarise(records=length(StationID)) 

output2 <- output %>%
  group_by(StationID) %>%
  ddply(c('SiteID','StationID'),summarise,records=length(StationID)) 

# Look through output2 results and subset StationID's that grabbed +1 geometry
df <- data.frame(StationID=character(),SiteID=numeric())
for(i in 1:nrow(output2)){
  if(output2$records[i]==1){
    print(i)
  }else{
    placehold <- data.frame(StationID=as.character(output2$StationID[i]),SiteID=i)
    df <- rbind(df,placehold)
    rm(placehold)}
}
# Join results to output dataframe to attach geometry information
df1 <- join(df,output,by=c('StationID','SiteID'))

# The Money loop: Loop with logic to deal with multiple geometries if encountered in buffering process
# This uses an automatic selection if a StationID has multiple geometries with the same WQS_ID
#  (it chooses the first entry) and a user selection component if a StationID has multiple
#  geometries with multiple WQS_ID's
sitelist <- list()
autoselectentry <- data.frame()
userselectentry <- data.frame()

# User input function, pops up when more than one geometry is identified for a single site and
#  requires user to select which one is correct prior to continuing
INPUT <- function(x){
  CHOICE <- NA
  w <- gbasicdialog(title=x)
  glabel(paste('Which ID305B is correct for ',siteascharacter,' ?')
         ,container=w,anchor=c(0,1))
  rb <- gradio(labellist2,selected=length(labellist)+1,handler = function(h,...) CHOICE <<- svalue(rb)
               ,container=w)
  visible(w, set=TRUE)
  return(CHOICE)
}


#i<-2

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
    }else{# Then WQS_ID is different for the same StationID so go to user selection
      # Select site that is issue
      site <- factor(unique(splitStationID[[i]]$StationID), levels=levels(output$StationID))
      num <- which(select@data$StationID %in% site) # Find plotting order of StationID in question
      # Select geometries that are in question
      geom <- splitStationID[[i]]$ID305B
      num2 <- which(WQS@data$ID305B %in% geom) # Link selected ID305B info to actual WQS geometry
      testshape <- WQS[num2,]
      ## ggmap prep
      testshape_prj_f <- fortify(spTransform(testshape,CRS("+init=epsg:4326"))) #need it in right projection to overlay google
      testshape@data$id <- row.names(testshape) #need to build link to data desired
      testshape_prj_f <- left_join(testshape_prj_f, testshape@data,by='id')
      # Print ggmap of stream geometries and site
      basemap <- ggmap(get_googlemap(center = c(lon = select[num,]$LONG_DD, lat = select[num,]$LAT_DD)
                                     ,zoom = 12),extent='device',legend='topleft')
      tryCatch({print(basemap + geom_path(aes(x=long,y=lat,colour=ID305B,group=group), data=testshape_prj_f,size=1.5) +
              geom_point(aes(x=select[num,]$LONG_DD,y=select[num,]$LAT_DD), size=4) +
              annotate('text',x=select[num,]@data$LONG_DD,y=select[num,]@data$LAT_DD+0.001
                       ,label=select@data$StationID[num]))})
      ############################# need another way to label? indicate PWS? ###################################
      # User geometry selection function
      # Must trick gradio into working at present by adding extra (blank) factor
      labellist <- geom
      labellist2 <- factor(geom, levels=c(levels(geom),'No Answer'))
      labellist2[length(labellist)+1] <- 'No Answer'
      siteascharacter <- as.character(site)
      user.selection <- INPUT("User Geometry Selection")
      
      # Select record from df1 that user wants to keep
      userselectentry <- rbind(userselectentry,df1[which(df1$ID305B %in% user.selection),]) # was df1[which(df1$ID305B %in% user.selection[i]),]
      # Save StationID to final output list
      sitelist[i] <- as.character(unique(df1[which(df1$ID305B %in% user.selection),]$StationID))
      rm(testshape_prj_f)
      dev.off()
      }
  }
  }
  # Create a final geometry output with only 1 stream geometry per site
  slimoutput <- filter(output,!(StationID %in% sitelist)) %>%
    rbind(autoselectentry) %>%
    rbind(userselectentry)
}
  








































# working automatic selection tool, separates df1 into list of df's based on StationID

if(length(unique(df1$StationID))>1){ 
  # If 1+ geometries identified in buffer steps then need to make a list of df's with unique 
  # StationID's to work through selecting 1 geometry either automatically (only 1 WQS_ID) or
  # by user selection
  splitdf1 <- split(df1,with(df1,interaction(SiteID,WQS_ID)),drop=T)
  for(i in 1:length(splitdf1)){ # Loop through list of dataframes based on unique StationID
    if(length(unique(splitdf1[[i]]$WQS_ID))==1){
      # In each dataframe, if only one WQS_ID then choose first entry in dataframe and
      # save entry to a new dataframe to later append back to main geometry output dataframe
      splitdfselect <- splitdf1[[i]] # automatically select first entry if WQS_ID are identical
      autoselectentry <- rbind(autoselectentry,splitdfselect[!duplicated(splitdfselect[,c('StationID','SiteID')]),])
      # save it to final output list
      sitelist[i] <- as.character(unique(splitdfselect$StationID))
    }
  }
  # Create a final geometry output with only 1 stream geometry per site
  slimoutput <- filter(output,!(StationID %in% sitelist)) %>%
    rbind(autoselectentry)
}else{ # This means A) there is one or more StationID with multiple stream geometries BUT none there is more than one unique identifier from WQS layer (WQS_ID) and need the 
  # user to choose which geometry the StationID should have connected to based on information from
  # WQS layer
  # make new dataframe for each unique StationID 
  #for(i in 1:length(unique(df1$SiteID))){
  #stuff <- df1[1,3:17]
  #assign(paste("placeholder",i,sep="_"),stuff) 
  #}
  #not right yet
  # need to split df1 into n dataframes based on unique StationID WITH rest of columns preserved
  
  
  for(i in 1:length(unique(df1$SiteID))){ # I originally had this as WQS_ID, not sure why bc would
    # duplicate for loop runs?
    #user.selection <- NA
    # Select site that is issue
    site <- factor(df$StationID[i], levels=levels(output$StationID))
    num <- which(select@data$StationID %in% site) # Find plotting order of StationID in question
    #plot(select[num,]);title(select@data$StationID[num])
    # select geometries that are in question
    geom <- df1$ID305B
    num2 <- which(WQS@data$ID305B %in% geom) # Link selected ID305B info to actual WQS geometry
    testshape <- WQS[num2,]
    
    ## ggmap prep
    testshape_prj_f <- fortify(spTransform(testshape,CRS("+init=epsg:4326"))) #need it in right projection to overlay google
    testshape@data$id <- row.names(testshape) #need to build link to data desired
    testshape_prj_f <- left_join(testshape_prj_f, testshape@data,by='id')
    
    basemap <- ggmap(get_googlemap(center = c(lon = select[num,]$LongitudeD, lat = select[num,]$LatitudeDD)
                                   ,zoom = 12),extent='device',legend='topleft')
    print(basemap + geom_path(aes(x=long,y=lat,colour=ID305B,group=group), data=testshape_prj_f,size=1.5) +
            geom_point(aes(x=select[num,]$LongitudeD,y=select[num,]$LatitudeDD), size=4) +
            annotate('text',x=select[num,]@data$LongitudeD,y=select[num,]@data$LatitudeDD+0.001
                     ,label=select@data$StationID[num]))
    # need another way to label? indicate PWS?
    
    # User geometry selection function
    labellist <- geom
    siteascharacter <- as.character(site)
    user.selection <- INPUT("User Geometry Selection")
    
    # Select record from df1 that user wants to keep
    df2 <- df1[which(df1$ID305B %in% user.selection[i]),]
    # Remove duplicated StationID records from output and add in user selection
    slimoutput <- filter(output,!StationID==site) %>%
      rbind(df2)
    rm(df2)
  }
  
}







































# Working GUI user selection and ggmap

  if(length(unique(df1$WQS_ID))==1){# this means connection made to more than one geometry but all 
  # have same WQS_ID, so go back to output, dissolve on SiteID such that only 1 record per site
  slimoutput <- output[!duplicated(output[,c('StationID','SiteID')]),]
}else{ # this means there is more than one unique identifier from WQS layer (WQS_ID) and need the 
  # user to choose which geometry the StationID should have connected to based on information from
  # WQS layer
  # make new dataframe for each unique StationID 
  #for(i in 1:length(unique(df1$SiteID))){
    #stuff <- df1[1,3:17]
    #assign(paste("placeholder",i,sep="_"),stuff) 
  #}
  #not right yet
  # need to split df1 into n dataframes based on unique StationID WITH rest of columns preserved

  
  for(i in 1:length(unique(df1$SiteID))){ # I originally had this as WQS_ID, not sure why bc would
                                          # duplicate for loop runs?
    #user.selection <- NA
    # Select site that is issue
    site <- factor(df$StationID[i], levels=levels(output$StationID))
    num <- which(select@data$StationID %in% site) # Find plotting order of StationID in question
    #plot(select[num,]);title(select@data$StationID[num])
    # select geometries that are in question
    geom <- df1$ID305B
    num2 <- which(WQS@data$ID305B %in% geom) # Link selected ID305B info to actual WQS geometry
    testshape <- WQS[num2,]
    
    ## ggmap prep
    testshape_prj_f <- fortify(spTransform(testshape,CRS("+init=epsg:4326"))) #need it in right projection to overlay google
    testshape@data$id <- row.names(testshape) #need to build link to data desired
    testshape_prj_f <- left_join(testshape_prj_f, testshape@data,by='id')
    
    basemap <- ggmap(get_googlemap(center = c(lon = select[num,]$LongitudeD, lat = select[num,]$LatitudeDD)
                                   ,zoom = 12),extent='device',legend='topleft')
    print(basemap + geom_path(aes(x=long,y=lat,colour=ID305B,group=group), data=testshape_prj_f,size=1.5) +
      geom_point(aes(x=select[num,]$LongitudeD,y=select[num,]$LatitudeDD), size=4) +
      annotate('text',x=select[num,]@data$LongitudeD,y=select[num,]@data$LatitudeDD+0.001
               ,label=select@data$StationID[num]))
# need another way to label? indicate PWS?
    
    # User geometry selection function
    labellist <- geom
    siteascharacter <- as.character(site)
    user.selection <- INPUT("User Geometry Selection")
  
    # Select record from df1 that user wants to keep
    df2 <- df1[which(df1$ID305B %in% user.selection[i]),]
    # Remove duplicated StationID records from output and add in user selection
    slimoutput <- filter(output,!StationID==site) %>%
      rbind(df2)
    rm(df2)
  }

}