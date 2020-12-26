# Download data for covid app
#    COVID data from the Covid tracking project
#    Google mobility and Google Trends
# Xijin Ge Xijin.Ge@sdstate.edu

##################################################
#  Read Covid data from the covid tracking project
##################################################
setwd( "C:/Users/Xijin.Ge/Google Drive/research/covid-19/src/")
# US data
url <-  "https://api.covidtracking.com/v1/us/daily.csv"
download.file(url, "COVID_Tracking_US.csv")

# State level data
url <-  "https://api.covidtracking.com/v1/states/daily.csv"
download.file(url, "all-states-history.csv")


###############################################
#  Google Mobility data
##############################################


url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
download.file(url, "Global_Mobility_Report.csv")

  mobility <- read.csv("Global_Mobility_Report.csv")
  
  mobility <- mobility %>%
    filter(country_region_code == "US") %>%
    filter(nchar(sub_region_2) == 0)   # remove county level data
  
  #remove the first two columns
  write.csv(mobility[, -c(1:2)], "US_mobility.csv", row.names = FALSE)


#####################################
#  Read in Google Trend data
####################################

  #Python program: Get_Google_trend_all.py 
  # needs list of states: US_states.csv
  
trend1 <- read.csv("GT_data_time1.csv") %>%
    mutate(id = paste0(date, Region))
  
trend2 <- read.csv("GT_data_time2.csv")  %>%
  mutate(id = paste0(date, Region)) %>%
  select(-isPartial) %>%
  select(-date) %>%
  select(-Region) 



trend3 <- read.csv("GT_data_time3.csv")  %>%
  mutate(id = paste0(date, Region)) %>%
  select(-isPartial) %>%
  select(-date) %>%
  select(-Region) 

combined <- inner_join(trend1, trend2, by = 'id')
combined <- inner_join(combined, trend3, by = 'id') %>%
  select(-id)

write.csv(combined, "GT_data_combined.csv")

  
#---------Google Trends across states

geo1 <- read.csv("US_states_trend_geo1.csv", row.names = 1)  
  
geo2 <- read.csv("US_states_trend_geo2.csv", row.names = 1)  

geo3 <- read.csv("US_states_trend_geo3.csv", row.names = 1)
geo4 <- read.csv("US_states_trend_geo4.csv", row.names = 1)  

geo5 <- read.csv("US_states_trend_geo5.csv", row.names = 1)  

geo6 <- read.csv("US_states_trend_geo6.csv", row.names = 1) 

statesGeo <- geo1 %>%
  inner_join(geo2, by = "geoCode") %>%
  inner_join(geo3, by = "geoCode") %>%
  inner_join(geo4, by = "geoCode") %>%
  inner_join(geo5, by = "geoCode") %>%
  inner_join(geo6, by = "geoCode") %>%
  mutate(geoCode = gsub("US-", "", geoCode))

write.csv(statesGeo, "StateGeo.csv", row.names = F)
  
  
