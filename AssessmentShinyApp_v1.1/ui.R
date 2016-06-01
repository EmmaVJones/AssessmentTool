library(shiny)
library(shinyBS)
library(shinyFiles)
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


shinyUI(
  navbarPage('Assessment Tool: Station Table Populator',
             tabPanel('Basic Tool',
                      sidebarPanel(
                        h4(strong('Instructions:')),
                        p('Please navigate to the directory with GIS data downloaded with the app and select a .csv file of 
                          stations.'),
                        h5(strong('Select GIS Data Directory')),
                        shinyDirButton('directory', 'Choose Directory', 'Please select a folder containing necessary GIS data.'),
                        fileInput('sites','Upload Sites',accept='.csv',width='100%'),
                        hr(),
                        # choose DEQ Regional Office
                        selectInput("regionalOffice"
                                    ,label = 'Select a VDEQ Regional Office'
                                    ,choices=c('','BRRO-L','BRRO-R','NRO','PRO','SWRO','TRO','VRO')
                                    ,selected=''),
                        hr(),
                        #run analysis button
                        p("Navigate to the 'Results Table' tab and click the 'Merge Sites with GIS Information' button after you have uploaded a .csv of stations."),
                        p("Calculation progress can be tracked in the upper right hand corner."),
                        actionButton('runButton','Merge Sites with GIS Information'),
                        hr(),
                        p("Check the 'Geometry Issues Table' to verify all sites latched to the only one ID305B. If the table
                          is populated, then you need to proceed to the 'Advanced Mapping' tab to manually select WQ Standards 
                          information. Otherwise, you may proceed directly to the 'Final Results' tab. "),width=3),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Input Table",tableOutput('inputTable')),
                          tabPanel("Results Table",dataTableOutput('resultsTable')),
                          tabPanel("Geometry Issues Table",dataTableOutput('outputTableIssues'))
                        )
                      )),
             tabPanel('Advanced Mapping',
                      column(3,
                             wellPanel( #sidebarPanel(
                        h4(strong('Instructions:')),
                        p("Click the 'Plot Problem Sites on Map' button after you have identified sites in the previous step that need further review."),
                        p("Calculation progress can be tracked in the upper right hand corner."),
                        actionButton('runButton2','Plot Problem Sites on Map'),
                        hr(),
                        p("Once the lower table is populated, select only the ID305B's would you like to keep for each site, then proceed 
                          to the User Selection Tab to review your results before moving to the Final Results Tab."),
                        downloadButton("downloadProblemSites","Download Problem Sites"))),#,width=3),
                      column(9, #mainPanel(
                        tabsetPanel(
                          tabPanel("Sites For Review",tableOutput('outputTableIssues_test'),
                                   p("These sites are identified from your original uploaded file because they either 1) attached to too many
                                     stream geometries (with differing WQ Standards information) within a buffered area 2) required a large (300 m) buffer
                                     to attach to stream geometries and should be reviewed or 3) did not attach to any streams within a large (300 m) buffer 
                                     and will need further review in a GIS.")),
                          tabPanel("Map",
                                   leafletOutput("issueMap"),
                                   hr(),
                                   h4("Based on the map, which ID305B's would you like to keep for each site?"),
                                   h5("Please select only the ID305B's you wish to keep. Unselected rows will be dropped from further analyses."),
                                   fluidRow(dataTableOutput('userSelectionTable'))),
                          tabPanel("User Selection",tableOutput("subsetTable"))
                        ))
                      
             ),
             tabPanel('Final Results',
                      sidebarPanel(
                        h4(strong('Instructions:')),
                        #merge results button
                        p('Click the Merge Results button if you had sites to manage in the Advanced Mapping tab.'),
                        actionButton('mergeTables','Generate Final Results'),
                        hr(),
                        #download results button
                        p('Click the Download Results button after you have completed and reviewed all analyses
                          to save the results to a location on your computer.'),
                        downloadButton("downloadResults","Download Final Results"),width=3),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Basic Tool Results",tableOutput('resultsTable2')),
                          tabPanel("Advanced Mapping Results",tableOutput('subsetTable2')),
                          tabPanel("Final Results",dataTableOutput('comboResults'))))),
             tabPanel('IR Stations Table',
                      column(2,wellPanel(
                        h4(strong('Instructions:')),
                        p("Navigate through the drop down list of StationID's to assign additional station information."),
                        p("You can review your work prior to downloading results in the 'Review' tab.")),
                        selectInput('stationID',label='StationID',choices= c("","FakeName1","fakename2"))),
                      column(9,
                             tabsetPanel(
                               tabPanel('Station Information'#,
                                         # bunch of select inputs
                                         ),
                               tabPanel('Review',dataTableOutput('review'))))
                      ),
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