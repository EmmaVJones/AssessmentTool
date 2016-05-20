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
#sites <- read.csv('data/sites_2009prob.csv')

shinyUI(
  navbarPage('Assessment Tool: Station Table Populator',
             tabPanel('Basic Tool',
                      sidebarPanel(
                        fileInput('sites','Upload Sites',accept='.csv',width='100%'),
                        #run analysis button
                        actionButton('runButton','Run Sites'),
                        p("Click the Run Sites button after you have uploaded a .csv of stations.
                          Depending on your internet connection, you might have to click the 'Results Table'
                          tab to coerce the calculations to begin."),
                        #download results button
                        downloadButton("downloadResults","Download Results")
                        ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Input Table",tableOutput('inputTable')),
                          tabPanel("Results Table",dataTableOutput('resultsTable')),
                          tabPanel("Geometry Issues Table",dataTableOutput('outputTableIssues'))
                        )
                      )),
             tabPanel('Advanced Mapping',
                      sidebarPanel(actionButton('runButton2','Run Problem Sites'),
                                   p('Click the Run Sites button after you have identified sites in the previous step that need further review.')),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Sites For Review",tableOutput('outputTableIssues_test')),
                          tabPanel("Map",leafletOutput("issueMap"))
                          
                        ))
                      
             ),
             tabPanel('About',fluidRow(column(6,
                                              h3("This app was created for the DEQ Assessors to automate the Stations Table 
                         building process."),
                                              h6("Please contact Emma Jones at emma.jones@deq.virginia.gov for additional 
                         information."))))
             ))

#leafletOutput('map305B'),