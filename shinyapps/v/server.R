# for plotting
library(ggplot2)
require(ggrepel)
library(tidyr) # for gather function
library(forcats) # ploting
library(forecast) # time series
library(lubridate) # for showing up time correctly
library(plotly)
library(chinamap)
library(maps)

function(input, output, session) {
    
    observe({  
        cityNamesProvince <- unique( x[input$selectProvince,]$city )
        ix <- match(cityNames, cityNamesProvince)

        updateSelectInput(session, "selectCity", NULL, choices = cityNames[!is.na(ix)] ) 
        if( input$selectProvince == entireCountry ) 
            updateSelectInput(session, "selectCity", NULL, choices = NULL )    
        
        })
    
    output$todayTotalTable <- renderTable(todayTotal,rownames = TRUE, colnames = TRUE, bordered = TRUE)

    #各个省 确诊 历史数  -------------------------------------------    
    output$confirmedByProvincesHistorical <- renderPlot({

            d2 <- summary(x)[,1:5]
            p <- ggplot(d2,
                        aes(time, as.numeric(cum_confirm), group=province, color=province)) +
                geom_point() + geom_line() +
                geom_text_repel(aes(label=province),  family="SimSun",data=d2[d2$time == time(x), ], hjust=1) +
                theme_gray(base_size = 14) + theme(legend.position='none') +
                xlab(NULL) + ylab(NULL) + 
                ggtitle(paste(entireCountry,  x$time,"更新") )         

        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })
    
    #省内各城市 确诊 历史数  -------------------------------------------    
    output$cities_in_proviences <- renderPlot({

        d <- x[input$selectProvince0, ]
        p <- ggplot(d,
               aes(time, as.numeric(cum_confirm), group=city, color=city)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=city), family="SimSun",data=d[d$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) + 
            ggtitle(paste(input$selectProvince0, "各市", x$time, "更新") )

        if(input$logScale) 
            p <- p + scale_y_log10() 
        p

    })

    #全国 当天 确诊 数  -------------------------------------------
    output$realTimeProvinceConfirmed <- renderPlot({

        d = y[]; d <- d[1:20, ]
        d$confirm=as.numeric(d$confirm)
        d$name = fct_reorder(d$name, d$confirm)
        
        # This is used to create spaces so the numbers on top of the bar shows up.
        maxN <- max(d$confirm) *1.5
        if(input$logScale) 
            maxN <- max(d$confirm) *10
        
        p <- ggplot(d, aes(name, confirm)) + 
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
            theme_gray(base_size=14) + 
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL) +
            theme(text = element_text(size=17, family="SimSun"),
                  axis.text.x = element_text(angle=0, hjust=1))  + 
            #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
            ggtitle(paste("确诊 (死亡)", gsub(" .*","", y$lastUpdateTime), "腾迅") ) +            
            expand_limits(y = maxN)
        #theme(plot.title = element_text(size = 18))
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }) 
    
    
    #省内各个城市当天确诊数  -------------------------------------------
    output$realTimeCityConfirmed <- renderPlot({
        d = y[input$selectProvince0,] 
        d$confirm=as.numeric(d$confirm)
        d$name = fct_reorder(d$name, d$confirm)
        
        # This is used to create spaces so the numbers on top of the bar shows up.
        maxN <- max(d$confirm) *1.5
        if(input$logScale) 
            maxN <- max(d$confirm) *10
        
        p <- ggplot(d, aes(name, confirm)) + 
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
            theme_gray(base_size=14) + 
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL) +
            theme(text = element_text(size=17, family="SimSun"),
                  axis.text.x = element_text(angle=0, hjust=1))  + 
            #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
            ggtitle(paste(input$selectProvince0, "确诊 (死亡)", gsub(" .*","", y$lastUpdateTime), "腾迅") ) +            
            expand_limits(y = maxN)
            #theme(plot.title = element_text(size = 18))
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }) 
    
    
    #世界各国分布图，现在的数据 -------------------------------------------
    output$realTimeCityConfirmedWorld <- renderPlot({
        d = y['global',] 
        d$confirm=as.numeric(d$confirm)
        d$name = fct_reorder(d$name, d$confirm)
        
        d <- d[1:20, ]
        
        # This is used to create spaces so the numbers on top of the bar shows up.
        maxN <- max(d$confirm) *1.5
        if(input$logScale) 
            maxN <- max(d$confirm) *20
        
        p <- ggplot(d, aes(name, confirm)) + 
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
            theme_gray(base_size=14) + 
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL) +
            theme(text = element_text(size=17, family="SimSun"),
                  axis.text.x = element_text(angle=0, hjust=1))  + 
            #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
            ggtitle(paste("各国确诊 (死亡)", gsub(" .*","", y$lastUpdateTime), "腾迅") ) +            
            expand_limits(y = maxN)
        #theme(plot.title = element_text(size = 18))
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }) 

    #全国细节 历史图 -------------------------------------------
    output$historicalChinaData <- renderPlot({

        d2 <- summary(x)[,2:5] %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) 

        
        dl <- d2 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point(size=3) + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) 

            p <- p + ggtitle(paste("全国总数", x$time, "更新") ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })
    
    
    
    #全国细节 历史图 Plotly-------------------------------------------
    output$historicalChinaDataPlotly <- renderPlotly({
        
        d2 <- summary(x)[,2:5] %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) 
        
        
        dl <- d2 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point() + geom_line() +
            #geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + 
            theme(legend.title = element_blank()) +
            xlab(NULL) + ylab(NULL) 
        
        p <- p + ggtitle(paste("全国总数", x$time, "更新") ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"))
        
    })
    
    #全国细节 历史图 增加-------------------------------------------
    output$historicalChinaDataAdd <- renderPlot({
        
        d2 <- summary(x)[,2:5] %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) 
        
        d3 <- d2[-1, ] %>%
            mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
            mutate(cum_dead = diff(d2$cum_dead)) %>%
            mutate(cum_heal = diff(d2$cum_heal))
        
        # add a row with zeros but with date; so that the two figures align
        d3 <- rbind(d2[1, ], d3)
        d3[1, 2:4] <- 0;
        
        dl <- d3 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point(size=3) + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) 
        
        p <- p + ggtitle(paste("全国每日新增", x$time, "更新") ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })

    #省 历史图 新增-------------------------------------------
    output$provienceHistoricalAdd <- renderPlot({
        d2 <- x[input$selectProvince0, ]  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( city, time, cum_confirm, cum_dead, cum_heal) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
            arrange( order(time)) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) 
        

        d3 <- d2[-1, ] %>%
            mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
            mutate(cum_dead = diff(d2$cum_dead)) %>%
            mutate(cum_heal = diff(d2$cum_heal)) 
        
        # add a row with zeros but with date; so that the two figures align
        d3 <- rbind(d2[1, ], d3)
        d3[1, 2:4] <- 0;
        
        dl <- d3 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point(size=3) + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) 
        
        p <- p + ggtitle(paste(input$selectProvince0, "新增", x$time, "更新") ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })
    
    #省 历史图  -------------------------------------------
    output$provienceHistorical <- renderPlot({
        d2 <- x[input$selectProvince0, ]  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( city, time, cum_confirm, cum_dead, cum_heal) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
            arrange( order(time)) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) 
        
        dl <- d2 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point(size=3) + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) 
        
        p <- p + ggtitle(paste(input$selectProvince0, "总数", x$time, "更新") ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })
            
    #城市细节 历史图 -------------------------------------------
    output$cities_in_proviences_selected <- renderPlot({

            d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                mutate(cum_dead = as.integer(cum_dead)) %>%
                select( time, cum_confirm, cum_dead, cum_heal) %>%
                mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                mutate( cum_heal = meanImput(cum_heal, 2)) 
            

            
            dl <- d2 %>%
                gather( type, count, cum_confirm:cum_heal) %>%
                mutate( type = recode_factor(type,
                                             cum_confirm = "确诊",
                                             cum_dead = "死亡",
                                             cum_heal = "痊愈"))

            p <- ggplot(dl,
                        aes(time, as.numeric(count), group=type, color=type)) +
                        geom_point(size=3) + geom_line() +
                        geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                        theme_gray(base_size = 14) + theme(legend.position='none') +
                        xlab(NULL) + ylab(NULL) 
            p <- p + ggtitle(paste(input$selectCity, "总数", x$time, "更新") )                    

        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })
    

    #城市细节 历史图 新增-------------------------------------------
    output$cities_in_proviences_selectedAdd <- renderPlot({
        
        d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( time, cum_confirm, cum_dead, cum_heal) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) %>%
            arrange(order(time) )
        
        d3 <- d2[-1, ] %>%
            mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
            mutate(cum_dead = diff(d2$cum_dead)) %>%
            mutate(cum_heal = diff(d2$cum_heal))     
        
        # add a row with zeros but with date; so that the two figures align
        d3 <- rbind(d2[1, ], d3)
        d3[1, 2:4] <- 0;
        
        dl <- d3 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point(size=3) + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) 

            p <- p + ggtitle(paste(input$selectCity, "新增", x$time, "更新") )                    

        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    })

    
    #城市细节 历史图 Plotly-------------------------------------------
    output$cities_in_proviences_selected_plotly <- renderPlotly({
            d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                mutate(cum_dead = as.integer(cum_dead)) %>%
                select( time, cum_confirm, cum_dead, cum_heal) 

        
        dl <- d2 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = "确诊",
                                         cum_dead = "死亡",
                                         cum_heal = "痊愈"))
        
        p <- ggplot(dl,
                    aes(time, count, group=type, color=type)) +
            geom_point() + geom_line() +
            #geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14)  + theme(legend.title = element_blank() ) +
            xlab(NULL) + ylab(NULL) 

            p <- p + ggtitle(paste(input$selectCity, x$time, "更新") )                    

        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"))
        
    })
    
    #全国确诊人数预测, 百分比预测-------------------------------------------    
    output$forecastConfirmedChange <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2)) 
        # missing data with average of neighbors
        d2$cum_confirm<- meanImput(d2$cum_confirm, 2)
        d2 <- d2[-(1:10), ] # remove the first 10 days as % change is huge
        
        confirmed <- ts(diff(d2$cum_confirm)/d2$cum_confirm[1:(nrow(d2)-1)]*100, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )
        
        forecasted <- forecast(ets(confirmed), input$daysForcasted)
        
        predictedNconfirmed = d2$cum_confirm[nrow(d2)]* increasesByPercentages(forecasted$mean)       
        plot(forecasted, xaxt="n", main="", 
             ylab = "全国确诊增加百分比(%)",
             xlab = paste0("预期全国确诊每天增加", round( mean( forecasted$mean ), 1 ),
                          "%，", input$daysForcasted, "天后达到", round(predictedNconfirmed,0) )            
             )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    })
    
    #全国确诊人数预测, 直接预测-------------------------------------------
    output$forecastConfirmedRaw <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))
        # missing data with average of neighbors
        d2$cum_confirm<- meanImput(d2$cum_confirm, 2)
        
        confirmed <- ts(d2$cum_confirm, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
        forecasted <- forecast(ets(confirmed), input$daysForcasted)
        plot(forecasted, xaxt="n", main="", 
             ylab = "全国确诊",
             xlab = paste0("预期", input$daysForcasted, "天后全国确诊 ", round(forecasted$mean[input$daysForcasted],0), ", 区间[",
                          round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
        )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }) 

    #全国死亡人数预测, 用百分比预测-------------------------------------------
    output$forecastDeadChange <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))       
        
        # missing data with average of neighbors
        d2$cum_dead <- meanImput(d2$cum_dead, 2)
        
        d2 <- d2[-(1:10), ] # remove the first 10 days as % change is huge
        
        dead <- ts(diff(d2$cum_dead)/d2$cum_dead[1:(nrow(d2)-1)]*100, # percent change
                     start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )


        
        forecasted <- forecast(ets(dead), input$daysForcasted)
        
#        predictedNdeaded = d2$cum_dead[nrow(d2)]* (1+ forecasted$mean[input$daysForcasted]/100)^input$daysForcasted 
        predictedNdead = d2$cum_dead[nrow(d2)]* increasesByPercentages(forecasted$mean)           
        plot(forecasted, xaxt="n", main="", 
             ylab = "死亡人数增加百分比(%)",
             xlab = paste0("预期全国死亡累计每天增加", round(mean(forecasted$mean),1),
                          "%，", input$daysForcasted, "天后达到 ", round(predictedNdead,0) )            
        )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    })
    
    #全国死亡人数预测, 直接预测-------------------------------------------
    output$forecastDeadRaw <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))        
        # missing data with average of neighbors
        d2$cum_dead <- meanImput(d2$cum_dead, 2)
        
        deaded <- ts(d2$cum_dead, # percent change
                     start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
        forecasted <- forecast(ets(deaded), input$daysForcasted)
        plot(forecasted, xaxt="n", main="", 
             ylab = "全国死亡人数",
             xlab = paste0("预期", input$daysForcasted, "天后全国死亡累计", round(forecasted$mean[input$daysForcasted],0), "，区间[",
                          round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
        )
        a = seq(as.Date(min(d2$time)), by="days", length= + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    })  
    
    #世界地图--------------------------------------------------
    output$worldMap <- renderPlot ({
        withProgress(message = '下载地图', value = 0, {
        incProgress(0.1)
        plot(y, 
             continuous_scale=FALSE,
             palette='Blues')
        })
    }, height = 800, width = 800)  

    #中国地图---------------------------------------------------
    output$ChinaMap <- renderPlot ({
        withProgress(message = '下载地图', value = 0, {
        incProgress(0.1)
        cn = get_map_china()
        incProgress(0.5)
        })
        plot(y, region='china', chinamap=cn,
             continuous_scale=FALSE,
             palette='Blues')

    }, height = 800, width = 800)   
    
    #省地图---------------------------------------------------
    output$provinceMap <- renderPlot ({
#        if(input$selectProvince0 %in% specialProvinces ) {
#            return(NULL) } else { 
        m = sf::st_read("map/shijie.shp")
        plot(y, region = input$selectProvince0, 
             chinamap = m,
             palette='Blues')  
        #}    
    }, height = 600, width = 800)       
}

