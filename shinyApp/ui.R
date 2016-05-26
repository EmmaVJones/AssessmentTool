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
                          tab to coerce the calculations to begin.")
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
                          tabPanel("Map",
                                   leafletOutput("issueMap"),
                                   hr(),
                                   fluidRow(
                                     column(3,
                                            h4("ID305B Selection"),
                                            selectInput('StationID', 'StationID', ' '),
                                            uiOutput('optionsID305B')
                                     )))
                          
                          
                        ))
                      
             ),
             tabPanel('Final Results',
                      sidebarPanel(
                        #download results button
                        downloadButton("downloadResults","Download Results"),
                        p('Click the Download Results button after you have completed and reviewed all analyses
                          to save the results to a location on your computer.')),
                      mainPanel(dataTableOutput('finalResultsTable'))),
             tabPanel('About',fluidRow(column(12,
                                              h5("This app was created for the DEQ Assessors to automate the Stations Table 
                                                 building process."),
                                              h5("Users need to input a .csv file of sites they wish to associate with water
                                                 quality standards with column headers matching the exampleCSV.csv file downloaded
                                                 with the original zip file of all necessary data. Users will follow the 
                                                 instructions on each page of the app and progress from the Basic Tool tab to
                                                 the Advanced Mapping tab (if any geometry issues are identified) and finally 
                                                 to the Final Results tab to download the data and store it locally. The app 
                                                 does not save data between user sessions, so please download all results upon 
                                                 finishing each analysis."),
                                              br(),
                                              h5("Please contact Emma Jones at emma.jones@deq.virginia.gov for troubleshooing
                                                 assistance and any additional information."))))
             ))