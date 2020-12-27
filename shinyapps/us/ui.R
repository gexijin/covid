# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 


#################################################
#  UI
#################################################
library(plotly)
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 related Google searches and mobility"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectState", NULL,
                        choices = stateNames,
                        selected = stateNames[1]),
            
            checkboxGroupInput("selectWords", 
                               h5("Frequences of Google searches:"), 
                               choices = keywords,
                               selected = keywords[1]),
            
            checkboxGroupInput("selectSeries", 
                               h5("COVID-19 Statistics in the US:"), 
                               choices = Disease,
                               selected = Disease[1]),
            
            checkboxGroupInput("selectMobility", 
                               h5("Google Mobility"), 
                               choices = Mobility,
                               selected = NULL),
            h6("*Select two to examine correlation*"),
            
            sliderInput("selectMA", h5("Moving average (days):"),
                        min = 1, max = 14, value = 7),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Pattern",
                    plotlyOutput("US_GT_Plot"), 
                    br(),br(),
                   # plotOutput("crossCorrelationPlot")
                ),
                tabPanel("Map",
                         plotlyOutput("stateMap"),
                         plotlyOutput("stateMap2")
                         ,plotlyOutput("stateScatter")
                ),
                tabPanel("Correlation"
                         ,plotOutput("corrMatrix")
                ),
                #tabPanel("Heatmap", 
                #         plotOutput("heatmap")
                #         ),
                tabPanel("Projection"
                         ,sliderInput("daysForcasted", h5( paste("Days to project from ", max(CTP$date)) )  ,
                                      min = 1, max = 21,
                                      value = 14)
                         ,tableOutput("predictedTable")
                         ,br()
                         ,plotOutput("stateProjectionNoSeasonal")  
                         ,br()
                         ,plotOutput("stateProjection")


                ),
                tabPanel("About"
                         ,h5("Accuracy not guaranteed.", style = "color:red")   
                         ,h5("Data sources:",
                             "Covid-19 statistis is from", a("the Covid tracking project.",href="https://covidtracking.com/"), 
                             "Google search frequencies are downloaded from", a("Google Trends", href="https://trends.google.com/trends/?geo=US"), 
                             "downloaded using the", a("pytrends API.",href="https://pypi.org/project/pytrends/"), "Mobility data is downloaded from",
                             a("Google Mobility.",href="https://www.google.com/covid19/mobility/")
                         )    
                         ,h5("For feedbacks or suggestions  please contact me via "
                             ,a("email ",href="mailto:xijin.ge@sdstate.edu?Subject=Coronavirus website" ), "or", 
                             a("Twitter.", href="https://twitter.com/StevenXGe"),
                             "My research interests are genomics, bioinformatics, and data science ",
                             a("(lab homepage).", href="http://ge-lab.org/"), 
                             "Source code on ", a("GitHub.",aref="https://github.com/gexijin/wuhan"), 
                             " I am not a epidemiologists or statistician, so be critical of my analyses.")                         
                         

                ) 
                
                
            )
        )
    )
    
    ,tags$head(includeScript("ga.js")) # tracking usage with Google analytics  
)

