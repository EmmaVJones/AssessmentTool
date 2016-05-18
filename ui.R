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
#sites <- read.csv('data/sites_2009prob.csv')

shinyUI(
  navbarPage('Assessment Tool: Station Table Populator',
             tabPanel('Basic Tool',
                      sidebarPanel(
                        fileInput('sites','Upload Sites',accept='.csv',width='100%'),
                        #run analysis button
                        actionButton('runButton','Run Sites'),
                        p('Click the Run Sites button after you have uploaded a .csv of stations'),
                        #download results button
                        downloadButton("downloadResults","Download Results")
                      ),
                      mainPanel(DT::dataTableOutput('outputTable'),
                                tableOutput('table'))
             ),
             tabPanel('Advanced Mapping',
                      sidebarPanel(leafletOutput("map", width="100%", height="100%")
                                   
                      )
             )
  ))

#leafletOutput('map305B'),