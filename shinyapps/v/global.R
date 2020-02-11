# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020
#install.packages("dplyr","ggplot2","shiny")   # already included with rocker/shiny-verse docker image
#install.packages(c("forcats","ggrepel","forecast","plotly","shinyBS","lubridate"))
# install.packages("remotes")
# remotes::install_github("GuangchuangYu/nCov2019")  # main data package
# remotes::install_github("GuangchuangYu/chinamap")   #Chinese map
# install.packages(c("sp","mapproj","maps","sf"))
entireCountry <- "全国各省"
options(scipen=999) # turn off scientific notation like 1e+06
#daysForcasted = 10
options(warn=-1) #suppress warnings
# Data see: https://mp.weixin.qq.com/s/lrQWGKj-mReWrxfi_4Sw9A
library(nCov2019)
library(dplyr)
library(tidyr) # for gather function 
y <- get_nCov2019()  # load real time data from Tencent
x <- load_nCov2019() #load historical data


# correct an erorr in data "四川 " "四川"
x$data$province <- gsub(" ", "", x$data$province)
x$data$city <- gsub(" ", "", x$data$city)

#Get a list of sorted provinces
provinceNames <- x$data %>% 
  filter(province == city) %>%
  arrange(province, desc(cum_confirm) ) %>%
  group_by(province) %>%
  filter(row_number() ==1) %>%
  arrange(desc(cum_confirm)) %>% 
  pull(province)

#provinceNames <- c(entireCountry, provinceNames)

# Get a list of cities sorted by cases
cityNames <- x$data %>% 
  filter(province != city) %>%
  arrange(city, desc(cum_confirm) ) %>%
  group_by(city) %>%
  filter(row_number() ==1) %>%
  arrange(desc(cum_confirm)) %>% 
  pull(city)

#Beijing, ...
specialProvinces <- names(sort(table(x$data$province))[1:10] )


# add province names to end of city names, as Beijing, Shanghai
cityNames = c(cityNames, provinceNames)
#Today's totals
todayTotal <- do.call(rbind, Map(data.frame, total=y$chinaTotal,add=y$chinaAdd))
colnames(todayTotal) <- c("总数","增加")
rownames(todayTotal) <- c("确诊","疑似","死亡","痊愈")

ChinaHistory <- summary(x)[, 2:5] %>% 
  mutate(cum_dead = as.integer(cum_dead)) %>%
  mutate(cum_confirm = replace_na(cum_confirm, 0),
         cum_dead = replace_na(cum_dead, 0),
         cum_heal = replace_na(cum_heal, 0)) %>%
  group_by(time) %>%
  summarise( cum_confirm = sum(cum_confirm),
             cum_dead = sum(cum_dead),
             cum_heal = sum(cum_heal)) %>%
  arrange(time)


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

#Given a set of percentage increase or decrease, calculate final
# not 10% is represented as 10, not 0.1
increasesByPercentages <- function(x ) {
  increase = 1;
  for (i in 1:length(x))
    increase <- increase * (1 + x[i]/100)
  increase
}


finc <- function(x) {
  # format increases
  # convert 235 -> "+235"
  #         -235 -> "-235"
  if(x > 0) {
    return( paste0("+",x) )
  } else{
    return( as.character(x) )
  } 
}
#  Below come from #https://shiny.rstudio.com/gallery/unicode-characters.html
# for displaying Chinese characters
# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
      system('locate wqy-zenhei.ttc') != 0 &&
      !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    curl::curl_download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    warning(
      "You probably want Chinese locale on Windows for this app",
      "to render correctly. See ",
      "https://github.com/rstudio/shiny/issues/1053#issuecomment-167011937"
    )
  }
}
