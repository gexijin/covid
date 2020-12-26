# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 


###############################################
#  server
###############################################

function(input, output, session) {

    mergedData <- reactive({
        # This function prepares data based on user selection
        # Returns a long form time-series data
        
        if(is.null(input$selectState)) 
            return(NULL)        

        ##########################################
        # merge Google trends, mobility, with cases
        ##########################################
        
        # filter Google Trends data based on selected state
        TrendRegion <- Trend %>%
            filter(Region == input$selectState) %>%  # "US", "AL"
            select(-Region)

        # filter Google mobility data based on selected state
        mobilityRegion <- mobility %>%
            filter( Region == input$selectState) %>%   
            select(-Region)
        
        # filter covid data by state
        if(input$selectState == 'US') {
            CTPRegion <- CTP 
        } else {
            CTPRegion <- CTPstate %>%
                filter(state == input$selectState) %>%
                select(-state)
        }
        
        #Merge mobility, search and infection data.
        merged <- inner_join(TrendRegion, CTPRegion, by="date")  %>%
            inner_join(mobilityRegion, by = 'date') %>%
            select(c(date, cases, hospitalized, death, positiveRate, nTests, ICU,
                     word1, word2, word3, word4, word5, word6, 
                     workplaces, transit, grocery, retail)) %>%
            gather("key", "value", -date) %>%     # convert to long form
            as_tsibble(key = key, index = date)   # convert to tsibble object

        # calculate moving averages     # https://davisvaughan.github.io/slider/
        merged3 <- merged %>% 
            group_by_key() %>% 
            mutate(Daily_MA = slide_dbl(value, 
                                        ~mean(.x, na.rm = TRUE),
                                        .before = input$selectMA - 1, # 7 day moving average
                                        .after = 0
            )) %>%
            mutate( key = recode(key, 
                                 word1 = keywords[1],
                                 word2 = keywords[2],
                                 word3 = keywords[3],
                                 word4 = keywords[4],
                                 word5 = keywords[5],
                                 word6 = keywords[6],
                                 hospitalized = Disease[1],
                                 cases = Disease[2],
                                 death = Disease[3],
                                 positiveRate = Disease[4],
                                 nTests = Disease[5],
                                 ICU = Disease[6],
                                 transit = Mobility[1],
                                 retail = Mobility[2],
                                 grocery = Mobility[3],
                                 workplaces = Mobility[4]
                                 ) )
        #write.csv(merged3, "Compiled_data.csv", row.names = FALSE)
        return(merged3)
        
    })
    
    output$US_GT_Plot_ggplotly <- renderPlotly({
        # changes of mobility, search and infection  over time for US and states
        if(is.null(mergedData()))
            return(NULL)
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) == 0 ) { # if not initialized yet
            return(NULL) 
            } else { 
                selectedData <- mergedData() %>%
                    filter(key %in% selected)   
                
                if(nrow(selectedData) < 1) { # if there is no data
                    return(NULL)
                } else { 
                
                p <-  selectedData %>%
                    autoplot(.vars = Daily_MA) +
                    ylab("Relative measure or %") +
                    theme(axis.title.x = element_blank()) +
                    theme(legend.title = element_blank()) +
                    theme(legend.position = "bottom")
        
                ggplotly(p)%>%
                    layout(legend = list(
                    orientation = "h", x = 0.4, y = -0.2
                    ) )
            } #else
        } #else
        
    })
    
    
    
    output$US_GT_Plot <- renderPlotly({
        # The changes of mobility, search, and infection over time for a region
        # using plot_ly. 
        # Based on https://ramikrispin.github.io/halloween-time-series-workshop/02-plotting-ts-objects/plotting_ts_objects.html
        if(is.null(mergedData()))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) == 0) 
            return(NULL) else { 
                selectedData <- mergedData() %>%
                    filter(key %in% selected) 

                if(nrow(selectedData) < 1) { # if there is no data
                    return(NULL)
                } else {                 
                
                    p <- plot_ly()
                    
                    for(i in selected ){
                        
                        df <- selectedData %>% 
                            filter(key == i) 
                        # assign values to NA if it is zero. Mostly for ICU
                       # df$Daily_MA[ df$Daily_MA == 0 ] <- NA
                        
                        p <- p %>%
                            add_lines(x = as.Date(df$date),
                                      y = df$Daily_MA,
                                      type = "scatter",
                                      mode = "lines",
                                      name = i)
                    }
                }
                
                p %>% layout(legend = list(orientation = "h"))
                
            }
        
    })
    
    
    
    output$crossCorrelationPlot <- renderPlot({
        # when two variables are chosen return a ccf plot
        if(is.null(mergedData()))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) != 2) 
            return(NULL) else { 
                
                selectedData <- mergedData() %>%
                    filter(key %in% selected)   
                
                if(nrow(selectedData) < 1) { # if there is no data
                    return(NULL)
                } else {             
                
                    var1 <- selectedData %>%
                        filter(key == selected[1]) %>%
                        pull(value)
                    var2 <- selectedData %>%
                        filter(key == selected[2]) %>%
                        pull(value)
                    
                    # compute cross correlation
                    # https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-correlation-within-and-among-time-series.html
                    ccf(var1, var2, 
                        lag.max = 21,
                        ylab = "Cross-correlation",
                        main = paste("Cross-correlation of", selected[1], "&", selected[2]),
                        ylim = c(-1, 1)
                        
                        )
    
                    
                    } 
                }
        
    })
    
    output$stateMap <- renderPlotly({
        # plots map using the usmap package
        
         if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) == 0) 
            return(NULL) else { 
                df <- mapData  # a temporary dataframe. Rename the selected column for plotting map
                ix <- grep(selected[1], nameKey) + 4 # 4 columns in the statepop 
                
                colnames(df)[ix] <- "value"
                #write.csv(df,"selected1.csv")
                p <- plot_usmap(data = df, 
                                values = "value", 
                                color = "red") + 
                     scale_fill_continuous( name = selected[1],
                                            low = "white", high = "red",
                                            label = scales::comma ) + 
                     theme(legend.position = "right")
                
                ggplotly(p, tooltip = c("value"))
                
            }
        
    })
    
    
    
    output$stateMap2 <- renderPlotly({
        # plots map using the usmap package
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) < 2) 
            return(NULL) else { 
                df <- mapData  # a temporary dataframe. Rename the selected column for plotting map
                ix <- grep(selected[2], nameKey) + 4 # 4 columns in the statepop 
                
                colnames(df)[ix] <- "value"
                #write.csv(df,"selected2.csv")
                p <- plot_usmap(data = df, 
                                values = "value", 
                                color = "red") + 
                    scale_fill_continuous( name = selected[2],
                                           low = "white", high = "red",
                                           label = scales::comma ) + 
                    theme(legend.position = "right")
                
                ggplotly(p, tooltip = c("value"))
                
            }
        
    })   

    
    output$stateScatter <- renderPlotly({
        # plots map using the usmap package for the 2nd column
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) < 2) 
            return(NULL) else { 
                
                # COVID numbers are log transformed. 1 is added as there are zeroes. 
                df <- mapData  %>% # a temporary dataframe. Rename the selected column for plotting map
                    mutate(hospitalized = log10( 1 + hospitalized)) %>%
                    mutate(cases = log10(1 + cases)) %>%
                    mutate(nTests = log10(1 + nTests)) %>%
                    mutate(death = log10(1 + death)) %>%
                    mutate(ICU = log10(1 + ICU))

                                    
                ix <- grep(selected[1], nameKey) + 4 # 4 columns in the statepop 
                colnames(df)[ix] <- "x"
                
                ix <- grep(selected[2], nameKey) + 4 # 4 columns in the statepop 
                colnames(df)[ix] <- "y"
                
                df <- df %>%
                    select(x, y, abbr) %>%
                    drop_na() %>%
                    filter( abs(x) > 1e-6 ) %>%
                    filter( abs(y) > 1e-6)
                
                
                
                df <- df[rowSums(df[, 1:2]) > 0, ]
                
                
                correlation <- cor.test(df$x, df$y)
                
                p <- ggplot(df, aes(x = x, y = y)) +  #size = pop_2015,
                    geom_point(aes(text=sprintf("%s", abbr))) +
                    xlab(selected[1]) +
                    ylab(selected[2]) +
                    geom_smooth(method='lm') +
                    annotate("text", label = paste0("R=", round(correlation$estimate, 2), 
                                                   " (P =", formatC(correlation$p.value, format = "e", digits = 2), ")"),
                             size = 3,
                             color = "red",
                             x = max(df$x) * 0.8,
                             y = min(df$y)
                             )
                
                ggplotly(p)
                
            }
        
    })

    output$corrMatrix <- renderPlot({
        # Correlation Matrix
        
        if(is.null(mergedData() ))
            return(NULL)
        
        temData <- mapData %>%
            select(-abbr, -fips, -pop_2015, -full, -word6)
        
        colnames(temData)[1:5] <- keywords[1:5]
        
        corrplot(cor(temData), 
                 method="color", 
                 type = "upper", 
                 addCoef.col="black",
                 tl.cex = 1,  # font size for labels
                 number.cex = 1,     # font size for numbers
                 #order = "hclust",   # order using hclust
                 cl.pos = "n", # No color label
                 diag = FALSE)  # no diagonal
        
    }, width = 700, height = 700)    
    
    output$heatmap <- renderPlot({
        # Correlation Matrix
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        
        temData <- mapData %>%
            select(-abbr, -fips, -pop_2015, -full, -word6) %>%
            mutate(hospitalized = log10(hospitalized)) %>%
            mutate(cases = log10(cases)) %>%
            mutate(nTests = log10(nTests)) %>%
            mutate(death = log10(death)) %>%
            as.matrix()
        
        row.names(temData) <- mapData$abbr
        
        colnames(temData)[1:5] <- keywords[1:5]
        
        temData <- temData[, selected]
        temData[ is.na(temData)] <- 0
        temData <- temData[ , colSums(is.na(temData)) == 0]
        

        heatmap(temData, scale = "column",   Colv = NA)
        
    }, width = 700, height = 700)  
    
    
    
    projectionData <- reactive ({
        
        if(is.null(input$selectSeries ))
            return(NULL)
        
        selected <- input$selectSeries
        
        if(length( selected ) < 1) 
            return(NULL) else {
                
                d2 <- CTPstateRaw
                
                # entire country
                if(input$selectState == "US") { 
                    d2 <- CTPraw 
                } else {
                    #selected state
                    d2 <- d2 %>%
                        filter( state == input$selectState)  
                }
                
                d2 <- d2 %>%               
                    rename(time = date) %>%
                    arrange(time) 
                
                ix <- grep(selected[1], nameKey)# 4 columns in the statepop
                iy <- grep(names(nameKey)[ix], colnames(d2)) 
                colnames(d2)[iy] <- "selected" 
                
                nRep = sum( d2$selected == d2$selected[2]) 
                if(nRep > 3) 
                    d2 <- d2[-(1:(nRep-3)),]
                
                np <- nrow(d2) # number of time points
                if(np > npMax)
                    d2 <- d2[(np-npMax+1):np,]
                
                #  if(sum( d2$selected > 5) <10)
                #   return(NULL)
                
                par(mar = c(6, 4, 0, 2))
                # missing data with average of neighbors
                d2$selected<- meanImput(d2$selected, 2)
                
                # convert to time series using frequency of 7, weekly
                confirm <- ts(d2$selected, frequency=7  )
                
                # forcast
                # https://otexts.com/fpp2/forecasting-decomposition.html
                
                
                forecasted <- stlf(confirm, 
                                   robust = TRUE, 
                                   h = input$daysForcasted, 
                                   method = "naive",
                                   #lambda="auto",
                                   #method='ets',
                                   #etsmodel = "AAN"
                                   #etsmodel = "MAM"    
                                   biasadj=TRUE,
                                   damped=FALSE
                                   )
                
                
                return(list(forecasted = forecasted, confirm = confirm, d2 = d2))
                
                
            }
    } )  
    
 
    
    output$stateProjection <- renderPlot ({
        
        if(is.null(input$selectSeries ))
            return(NULL)
        selected <- input$selectSeries
                
                par(mar = c(6, 4, 0, 2))
                d2 <- projectionData()$d2
                confirm <- projectionData()$confirm
                forecasted <- projectionData()$forecasted
             
                
                # convert to time series using frequency of 7, weekly
                confirm <- ts(d2$selected, frequency=7  )
                
                # forcast
                # https://otexts.com/fpp2/forecasting-decomposition.html
                
                #ggplot2 version, cannot make date as x-axis label
                if(0){ 
                    autoplot(forecasted) +
                        ylab( selected[1] ) +
                        xlab( paste0(input$selectState, 
                                     " is expected to have ",
                                     round(forecasted$mean[input$daysForcasted],0), 
                                     " ", selected[1], " on ", 
                                     format( as.Date(max(d2$time)) + input$daysForcasted, "%b %d"  ),  
                                     ". 95% CI [",
                                     round(forecasted$lower[input$daysForcasted],0), "-",
                                     round(forecasted$upper[input$daysForcasted],0),"]."
                        )    ) 
                }
                
                
                plot(forecasted, xaxt="n", main="", 
                     ylab = paste0(selected[1], " in ", input$selectState)  )
                
                #construct date sequence
                # https://stackoverflow.com/questions/50924044/decimal-date-to-date-time-using-lubridate-for-daily-time-series-created-by-ts
                a = seq(as.Date(min(d2$time)), 
                        by="days", 
                        length=input$daysForcasted + nrow(d2)  )
                
                # add axis labels in Date format
                axis(1, #below axis
                     las = 3,
                     at = c( time(confirm), time(forecasted$mean)),  # orginal + forecast, 1, 1.1428, 1.28..
                     labels = format(a, "%b %d"))   # Dec. 1
                
            
    })      
    
    output$predictedTable <- renderTable({
        if(is.null(input$selectSeries ))
            return(NULL)
        
        if(is.null(projectionData() ))
            return(NULL)
        
        d2 <- projectionData()$d2
        confirm <- projectionData()$confirm
        forecasted <- projectionData()$forecasted
        
        a = seq(as.Date(max(d2$time)) + 1, by="days", length=input$daysForcasted  )
        tb <- cbind(forecasted$mean, forecasted$lower[, 2], forecasted$upper[, 2])
        
        tb[tb < 0] <- 0 # replace negative values as zero
        # format 1000 separators
        #https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
        tb <- format(round(tb, 0), nsmall=0, big.mark=",")  # 1,000.6
        tb <- cbind(format(a, "%b. %d"), 
                    as.character( wday(a, label=TRUE)),
                    tb)
        
        colnames(tb) <- c("Date", "Day", input$selectSeries[1], "Lower 95%", "Upper 95%")
        
        tb
        
    }, include.rownames=FALSE, striped=TRUE, bordered = TRUE, width = "auto", hover=TRUE, align = "r")
    
}
