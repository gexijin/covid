# Download data for covid app
#    COVID data from the Covid tracking project
#    Google mobility and Google Trends
# Xijin Ge Xijin.Ge@sdstate.edu

library(tsibble)
library(fabletools)  # plot time series
library(dplyr)
library(tidyr)
library(lubridate)
library(slider) # for moving average
library(plotly)
library(usmap)   # map ploting
library(ggplot2)
library(corrplot) # for correlation plot
library(feather) # for faster loading of data frames

# this is where the script and the data files are located in the container.
setwd("/srv/data/update/")

beginning = as.Date("2020-03-1")

##################################################
#  Read Covid data from the covid tracking project
##################################################
#setwd( "C:/Users/Xijin.Ge/Google Drive/research/covid-19/src/")
# US data
url <-  "https://api.covidtracking.com/v1/us/daily.csv"
download.file(url, "COVID_Tracking_US.csv")

# https://api.covidtracking.com/v1/us/daily.csv
CTP <- read.csv("COVID_Tracking_US.csv")

CTPraw <- CTP %>%
  mutate(positiveRate = positiveIncrease / (1 + totalTestResultsIncrease) *100 ) %>%
  select(date, positiveIncrease, hospitalizedCurrently, deathIncrease,
         positiveRate, totalTestResultsIncrease, inIcuCurrently ) %>%
  rename(cases = positiveIncrease ) %>%
  rename(hospitalized = hospitalizedCurrently ) %>%
  rename(death = deathIncrease ) %>%
  rename(nTests = totalTestResultsIncrease ) %>%
  rename(ICU = inIcuCurrently ) %>%
  replace_na(list(cases = 0, hospitalized = 0, death = 0, ICU = 0)) %>%
  mutate(date = as.Date(as.character(date), tryFormats = "%Y%m%d")) %>%
  arrange(date) %>%     #sort
  filter(date >= beginning )   # remove earlier 

write_feather(CTPraw, "CTPraw.feather")

# some states ICU numbers are all zero
CTP <- CTPraw %>%
  mutate(cases = cases / (1e-6 + max(cases)) *100 ) %>%   
  mutate(hospitalized = hospitalized / ( 1e-6 + max(hospitalized) ) *100 ) %>%   
  mutate(death = death / (1e-6 + max(death) ) *100 ) %>%
  mutate(ICU = ICU / (1e-6 + max(ICU, na.rm = TRUE)) *100 ) %>%  
  mutate(nTests = nTests / (1e-6 + max(nTests)) *100 ) 
write_feather(CTP, "CTP.feather")

# State level data
url <-  "https://api.covidtracking.com/v1/states/daily.csv"
download.file(url, "all-states-history.csv")

# https://api.covidtracking.com/v1/states/daily.csv
CTPstate <- read.csv("../../data/us/all-states-history.csv")

CTPstateRaw <- CTPstate %>%
  mutate(positiveRate = positiveIncrease / (negativeIncrease + positiveIncrease) *100 ) %>%
  select(date, state, positiveIncrease, hospitalizedCurrently, 
         deathIncrease, positiveRate, totalTestResultsIncrease, inIcuCurrently ) %>%
  rename(cases = positiveIncrease ) %>%
  rename(hospitalized = hospitalizedCurrently ) %>%
  rename(death = deathIncrease ) %>%
  rename(nTests = totalTestResultsIncrease ) %>%
  rename(ICU = inIcuCurrently ) %>%
  replace_na(list(cases = 0, hospitalized = 0, death = 0, ICU = 0)) %>%
  mutate(date = as.Date(as.character(date), tryFormats = "%Y%m%d")) %>%
  arrange(date) %>%     #sort
  filter(date >= beginning)  # remove earlier 
write_feather(CTPstateRaw, "CTPstateRaw.feather")

CTPstate <- CTPstateRaw %>%    
  group_by(state) %>%
  mutate(cases = cases / (1e-6 + max(cases, na.rm = TRUE)) *100 ) %>%   
  mutate(hospitalized = hospitalized / (1e-6 + max(hospitalized, na.rm = TRUE)) *100 ) %>%   
  mutate(death = death / (1e-6 + max(death, na.rm = TRUE)) *100 ) %>%
  mutate(ICU = ICU / (1e-6 + max(ICU, na.rm = TRUE)) *100 ) %>%
  mutate(nTests = nTests / (1e-6 + max(nTests, na.rm = TRUE)) *100 ) %>%
  ungroup()
write_feather(CTPstate, "CTPstate.feather")

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

  mobility <- read.csv("US_mobility.csv")
  mobility <- mobility %>%
    mutate(date = as.Date(date))  %>%
    rename(Region = iso_3166_2_code) %>%
    mutate( Region =      # missing iso code is imputed with US
              replace(Region, nchar(Region) <2, "US") ) %>%
    mutate(Region = gsub("US-", "", Region)) %>%
    filter(date >= beginning) 
  
  mobility <- mobility[, -c(1:3,5)]
  colnames(mobility) <- gsub("_.*", "", colnames(mobility))  # shorten col names
  
  mobility <- mobility %>%
    select(c(Region, date, transit, workplaces, retail, grocery)) %>% 
    mutate(transit = 1 * transit) %>%   # reverse direction -10 means 10% decrease in mobility
    mutate(workplaces = 1 * workplaces) %>%
    mutate(retail = 1 * retail) %>%
    mutate(grocery = 1 * grocery) 
  write_feather(mobility, "mobility.feather")
  

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

Trend <- read.csv("GT_data_combined.csv") %>% 
  rename(word1 = covid.symptoms) %>% # recode the column names from search keyword
  rename(word2 = covid.testing) %>%
  rename(word3 = loss.of.smell) %>%
  rename(word4 = loss.of.taste) %>%
  rename(word5 = covid) %>%
  rename(word6 = iphone) %>%
  mutate(date = as.Date(date) ) %>% 
  select(-isPartial) %>%
  group_by(Region) %>%   # for normlization using the max in state
  mutate(word1 = word1 / max(word1) *100 ) %>%  # normalized 
  mutate(word2 = word2 / max(word2) *100 ) %>%
  mutate(word3 = word3 / max(word3) *100 ) %>%
  mutate(word4 = word4 / max(word4) *100 ) %>%
  mutate(word5 = word5 / max(word5) *100 ) %>%
  mutate(word6 = word6 / max(word6) *100 ) %>%
  mutate(Region = gsub("US-", "", Region)) %>%     #US-AL  --> AL
  ungroup()  # this is needed; otherwise warnning.

write_feather(Trend, "Trend.feather")

  
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


statesGeo <- read.csv("StateGeo.csv")

statesGeo <- merge(statepop, statesGeo, by.x = 'abbr', by.y= 'geoCode' )
write_feather(statesGeo, "statesGeo.feather")  



###########################################################
#  Merge interest by state with COVID statistics for map
###########################################################

# filter Google mobility data based on selected state
mobilityRegion <- mobility %>%
  group_by(Region) %>%
  arrange(desc(date)) %>%
  slice(1) %>%  # only keep the most recent data
  ungroup() %>%
  select(-date) %>%
  rename(abbr = Region)


# calculate the moving average of all statistics then keep the most recent
CTPlatest <- CTPstateRaw %>%
  arrange(date) %>%    #sort from old to new
  group_by(state) %>%
  mutate(cases = slide_dbl(cases, 
                           ~mean(.x, na.rm = TRUE),
                           .before = 7, # 7 day moving average
                           .after = 0     )) %>% 
  mutate(hospitalized = slide_dbl(hospitalized, 
                                  ~mean(.x, na.rm = TRUE),
                                  .before = 7, # 7 day moving average
                                  .after = 0     )) %>% 
  mutate(death = slide_dbl(death, 
                           ~mean(.x, na.rm = TRUE),
                           .before = 7, # 7 day moving average
                           .after = 0     )) %>% 
  mutate(positiveRate = slide_dbl(positiveRate, 
                                  ~mean(.x, na.rm = TRUE),
                                  .before = 7, # 7 day moving average
                                  .after = 0     )) %>% 
  mutate(nTests = slide_dbl(nTests, 
                            ~mean(.x, na.rm = TRUE),
                            .before = 7, # 7 day moving average
                            .after = 0     )) %>% 
  mutate(nTests = slide_dbl(ICU, 
                            ~mean(.x, na.rm = TRUE),
                            .before = 7, # 7 day moving average
                            .after = 0     )) %>% 
  arrange(desc(date)) %>%     #most recent on top
  slice(1) %>%  # only keep the most recent data
  ungroup() %>%
  rename(abbr = state) %>%
  select(-date)


#Merge mobility, search and infection data.
mapData <- inner_join(statesGeo, CTPlatest, by="abbr")  %>%
  inner_join(mobilityRegion, by = 'abbr') %>%
  mutate(cases = cases *100000/ pop_2015) %>%    # normalize by population
  mutate(hospitalized = hospitalized *100000/ pop_2015) %>%
  mutate(death = death *100000/ pop_2015) %>%
  mutate(nTests = nTests *100000/ pop_2015) %>%
  mutate(ICU = ICU *100000/ pop_2015) %>%
  rename(word1 = covid.symptoms) %>% # recode the column names from search keyword
  rename(word2 = covid.testing) %>%
  rename(word3 = loss.of.smell) %>%
  rename(word4 = loss.of.taste) %>%
  rename(word5 = covid) %>%
  rename(word6 = iphone)

mapData[is.na(mapData)] <- 0
write_feather(mapData, "mapData.feather")
  
