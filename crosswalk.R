
inputFile() == sites <- read.csv('data/sites_2009prob_SLIM.csv')
initialResults() == slimoutput
tableIssues() == tableIssues_tbl
tableIssues_shptbl() ==problemsites_tbl

tableIssues_tbl <- subset(slimoutput, slimoutput$Comment %in% c("Site Attached to 1+ Stream Geometry <50 m Buffer; See Advanced Mapping Tab","Use GIS for this site","Site Buffered 300 m; Review Results in Advanced Mapping Tab"))

problemsites_tbl <- subset(sites,StationID %in% unique(tableIssues_tbl$StationID))

geometries <- tableIssues_tbl()$ID305B

problemsites <- subset(sites,StationID %in% unique(tableIssues_tbl$StationID)) %>%
  join(tableIssues_tbl[,17:18],by='StationID') %>%
  unique()



geometries <- tableIssues_tbl$ID305B
num_geometries <- which(WQS_p@data$ID305B %in% geometries)
WQS_p_selection <- WQS_p[num_geometries,]

WQS_ID305B <- WQS_p_selection@data

WQS_ID305B2 <- unique(WQS_ID305B) %>%
  join(tableIssues_tbl,by='ID305B')


# mess with splitting DF to get updated ID305B section to work
split_tableIssues_tbl <- split(tableIssues_tbl,tableIssues_tbl$StationID,drop=T)
length(split_tableIssues_tbl[[1]]$StationID)
split_tableIssues_tbl$StationID
testlist <- for(i in 1:length(split_tableIssues_tbl)) {
                x <- split_tableIssues_tbl[[1]]$StationID
                return(x)
  }


# need to take StationID's from problemsites (see line 13 to match in server code) for names in drop down list
# need to take ID305B options from tableIssues() bc that has all ID305B's