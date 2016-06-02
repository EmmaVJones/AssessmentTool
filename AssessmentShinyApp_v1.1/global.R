STA_TYPE_CODE <- data.frame(STA_TYPE_CODE=c('A','B','C','C2','CB','CB-B','CMON','CR','EPA','FOSR','FPM','L','MAIA','MU','NONA','OWML','PA','PFWQ'
                                            ,'SNP','SS','TE','TM','TOX','TR','USFS','USFW','USGS','VDHB')
                            ,STA_TYPE_DESC=c('DEQ Ambient Monitoring Station','DEQ Biological Monitoring Station','DEQ Fish Tissue Monitoring Station'
                                             ,'Ambient Estuarine Probabilistic Monitoring Station','Chesapeake Bay Program Monitoring Station','Chesapeake Bay Estuarine Benthic Probabilistic Monitoring Station'
                                             ,'Citizen Monitoring Station','Citizen Requested (follow-up) Station','Environmental Protection Agency Monitoring Station'
                                             ,'Friends of the Shenandoah River','DEQ Freshwater Probabilistic Monitoring Station','DEQ Lake Monitoring Station','Mid Atlantic Integrated Assessment Monitoring Station'
                                             ,'Municipal Monitoring Station','Non-DEQ agencies, industries, organizations not already listed'
                                             ,'Occoquan Watershed Monitoring Lab Station','DEQ Probabilistic Ambient Monitoring Station','DEQ Pfiesteria Monitoring Station','Shenandoah National Park','DEQ Special Studies Monitoring Station'
                                             ,'DEQ Tidal Embayment Monitoring Station (special study)','DEQ TMDL Monitoring Station (special study)','Chesapeake Bay Special Toxics Monitoring Station','DEQ Trend Station','United States Forest Service Monitoring Station'
                                             ,'United States Fish and Wildlife Service Monitoring Station','United States Geological Survey Monitoring Station','Virginia Department of Health BEACH Monitoring Station'))
tblkp_AMB_STAT_CODES <- data.frame(AMB_STAT_CODE=c('','IM','IN','IN/O','NA','O','S','W')
                            ,AMB_STAT_DESC=c('None','Impaired','Insufficient Data','Insufficient Data with Observed Effects'
                                             ,'Not Applicable','Observed Effects','Supporting','Not Assessed'))
tblkp_BIO_STAT_CODES <- data.frame(BIO_STAT_CODE=c('','FS','HP','IM','IN','J','LP','MP','NA','UI','UN','UO','W')
                                   ,BIO_STAT_DESC=c('None','Fully Supporting VSCI or CPMI'
                                                    ,'Citizen Monitoring - High Probability for Adverse Conditions (Insufficient Information but having Observed Effects)'
                                                    ,'Impaired for VSCI or CPMI','Insufficient Data','Reserve Judgment / BPJ'
                                                    ,'Citizen Monitoring - Low Probability for Adverse Conditions (Insufficient Information but indicating Fully Supporting)'
                                                    ,'Citizen Monitoring - Medium Probability for Adverse Conditions (Insufficient Information but having Observed Effects)'
                                                    ,'Not Applicable','USFS - Impaired','USFS - Not Impaired','USFS - Slight Impairment (Observed Effects)','Not Assessed'))
