# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 

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

#library(ComplexHeatmap) # for heat map
#library(circlize)   # for heatmap

library(forecast) # time series

npMax <- 70  # only use last 5 weeks of data for forecast
plotWidth = 800
nPoints = 7
beginning = as.Date("2020-03-1")
#####################################
#  Read in Google Trend data
####################################
#setwd( "C:/Users/Xijin.Ge/Google Drive/research/covid-19/src/GT")
#------------Interest over time
Trend <- read.csv("../../data/us/GT_data_combined.csv") %>% 
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


#------------Interest by region
statesGeo <- read.csv("../../data/us/StateGeo.csv")

statesGeo <- merge(statepop, statesGeo, by.x = 'abbr', by.y= 'geoCode' )



##################################################
#  Read Covid data from the covid tracking project
##################################################

### US. Data from the COVID Tracking project------------------------------------------------
# https://api.covidtracking.com/v1/us/daily.csv
CTP <- read.csv("../../data/us/COVID_Tracking_US.csv")

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

# some states ICU numbers are all zero
CTP <- CTPraw %>%
    mutate(cases = cases / (1e-6 + max(cases)) *100 ) %>%   
    mutate(hospitalized = hospitalized / ( 1e-6 + max(hospitalized) ) *100 ) %>%   
    mutate(death = death / (1e-6 + max(death) ) *100 ) %>%
    mutate(ICU = ICU / (1e-6 + max(ICU, na.rm = TRUE)) *100 ) %>%  
    mutate(nTests = nTests / (1e-6 + max(nTests)) *100 ) 


### State-level Data from the COVID Tracking project---------------------------------------
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
    
CTPstate <- CTPstateRaw %>%    
    group_by(state) %>%
    mutate(cases = cases / (1e-6 + max(cases, na.rm = TRUE)) *100 ) %>%   
    mutate(hospitalized = hospitalized / (1e-6 + max(hospitalized, na.rm = TRUE)) *100 ) %>%   
    mutate(death = death / (1e-6 + max(death, na.rm = TRUE)) *100 ) %>%
    mutate(ICU = ICU / (1e-6 + max(ICU, na.rm = TRUE)) *100 ) %>%
    mutate(nTests = nTests / (1e-6 + max(nTests, na.rm = TRUE)) *100 ) %>%
    ungroup()

#write.csv(CTPstate, "tem.csv")
###############################################
#  Google Mobility data
##############################################

mobility <- read.csv("../../data/us/US_mobility.csv")
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

# column names
Mobility <- c("Transit Station", "Workplaces", "Retail & Recreation", "Grocery & Phamarcy")

Disease <- c("Confirmed cases", "Hospitalization",   
             "Deaths", "%Positive Tests", "N. of Tests","In ICU" )

keywords <- c("\"COVID symptoms\"", 
              "\"COVID testing\"", 
              "\"Loss of smell\"", 
              "\"Loss of taste\"", 
              "\"COVID\"", 
              "\"iPhone\"")

# !!! Note that the same keywords and in the same 
# order are used in inerest over time data and the geographical data. !!!

# prepare list of state names
stateNames = c("US", state.abb)
names(stateNames) = c("US", state.name)


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

#write.csv(mapData, "mapData.csv")

nameKey = list(   
    word1 = keywords[1],
    word2 = keywords[2],
    word3 = keywords[3],
    word4 = keywords[4],
    word5 = keywords[5],
    word6 = keywords[6],
    cases = Disease[1],    
    hospitalized = Disease[2],
    death = Disease[3],
    positiveRate = Disease[4],
    nTests = Disease[5],
    ICU = Disease[6],
    transit = Mobility[1],
    workplaces = Mobility[2],  
    retail = Mobility[3],
    grocery = Mobility[4]

)


# missing data imput using the mean of n neighboring data points on both sides
# if n = 1, then two neighbors, if n=2 then 2 neighbors on both sides
meanImput <- function (x, n = 2) { 
    ix <- is.na(x)
    x2 <- x
    for( ixx in which(ix)) {
        start <- ixx-n;
        if(start < 1) 
            start <- 1;
        end <- ixx + n;
        if(start > length(x)) 
            start <- length(x);  
        x2[ixx] <- mean( x[ start:end], na.rm = T  ) }
    return( x2 )
}
