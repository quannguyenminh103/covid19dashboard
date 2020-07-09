# install.packages('shiny')
# install.packages('Hmisc')
# install.packages('backports')
# install.packages('dplyr')
# install.packages('magick')
# install.packages('expm')
# install.packages('tidyverse')
# install.packages('spdplyr')
# install.packages('lubridate')
# install.packages('httr')
# install.packages('rgdal')
# install.packages('viridis')
# install.packages('devtools')
# install.packages('packcircles')
# install.packages('ggiraph')
# install.packages('giphyr')
# install.packages('RColorBrewer')
# install.packages('rsconnect')
# install.packages('mapproj')
# install.packages('RCurl')
# install.packages('lobstr')
# install.packages('profvis')
library(shiny)
library(shinydashboard)
library(magick)
library(ggplot2)
library(maps)
#library(plyr)
library(dplyr)
library(Hmisc)
library(stringr)
library(giphyr)
library(mapproj)
library(ggthemes)
library(plotly)
library(lubridate)
library(spdplyr)
library(tidyverse)
library(viridis)
library(devtools)
library(png)
#library(purrr)
library(packcircles)
library(ggiraph)
library(tableHTML)
library(leaflet)
library(sf)
library(data.table)


source('./tabletop10.R')
source('./livelyPDH.R')
#source('./newMap.R')
source('./rank_by_state.R')
source('./lineChart.R')
source('./eventUSMap.R')
source('./multiplelinecharts.R')
source('./demographicsData.R')

overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)

USdataTracking = read.csv(url('https://covidtracking.com/api/v1/us/daily.csv'))
### OVERVIEW TABS:
overviewData <- rbind(data.frame(county_resident = "Georgia", t(colSums(overviewData[, -c(1,5)])), case_rate = mean(overviewData$case_rate)),overviewData)

county_list <- unique(overviewData$county_resident)

###  ASSEMBLE THE WEBSITE CONTENTs

sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Statistics Maps", icon = icon("map"), tabName = "maps",
             badgeLabel = "Time", badgeColor = "green"),
    menuItem("MAGE Model Charts", icon = icon("chart-line"), tabName = "mage",
             badgeLabel = "Highlight", badgeColor = "green"),
    menuItem("COVID-19 by Age, Race & Gender", icon = icon("layer-group"), tabName = 'factors'),
    menuItem("Data Resources", icon = icon("file-code-o"), tabName = 'data')
  )
)

body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 15
  ),
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(
              column(4,selectInput("county", "Choose a county: ",
                                   county_list))
            ),
            fluidRow( class ='text-center',
                      #tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                      valueBoxOutput("PositiveBox", width = 3),
                      valueBoxOutput("DeathBox", width = 3),
                      valueBoxOutput("DeathRatioBox", width = 3),
                      valueBoxOutput("HospitalizationBox", width = 3)
            ),
            fluidRow( class = 'text-center',
                      valueBoxOutput("USPositiveBox", width = 3),
                      valueBoxOutput("USDeathBox", width = 3),
                      valueBoxOutput("USDeathRatioBox", width = 3),
                      valueBoxOutput("USHospitalizationBox", width = 3),
                      
                      
            ),
            fluidRow(class = 'text-center',
                     box(dataTableOutput('TopTable')),
                     box(plotlyOutput('dailyGrowth', height = 350)),
                     box(plotlyOutput('StateRank', height = 350))
            )
    ),

    tabItem(tabName = "maps",
            fluidRow(
              box(width = 4, leafletOutput('PositiveMap')),
              box(width = 4,leafletOutput('DeathMap')),
              box(width = 4,leafletOutput('HospitalizationMap'))
            ),
            fluidRow(
              box(width = 12, img(src="GAConfirmedCasesMap.gif"), align = 'center')
            ),

            fluidRow(
              box(width = 12, numericInput('number',"Choose a number of people",10,min = 1), align = 'center'),
            ),
            fluidRow(),
            fluidRow(
              box(width = 12, leafletOutput('RiskyMap', height = "800px"), align = 'center',height = 800)
            )
    ),
    tabItem(tabName = 'factors',
            fluidRow(
              box(plotlyOutput('Age')),
              box(plotlyOutput('Gender'))
            ),
            fluidRow(
              box(ggiraphOutput("Race")),
              #column(6,div(style="width:400px;height:800px;", ggiraphOutput("Race"))),
              box(plotlyOutput('Combined'), height = 530)
            )
    ),
    tabItem(tabName = 'mage',
            fluidRow(
              box(plotlyOutput('SusceptPro')),
              box(plotlyOutput('NCperday'))
            ),
            fluidRow(
              box(plotlyOutput('DeathPro')),
              box(plotlyOutput('Dperday'))
            ),
            fluidRow(
              box(plotlyOutput('Hospitalization')),
              box(plotlyOutput('ICU'))
            )
    ),
    
    tabItem(tabName = 'data',
            tags$style(make_css(list('.box', 
                                     c('font-size', 'font-family', 'color'), 
                                     c('16px', 'georgia', 'black')))),
            fluidRow(box(uiOutput('OVData'))),
            fluidRow(box(uiOutput("todayData"))),
            fluidRow(box(uiOutput('dataTracking'))),
            fluidRow(box(uiOutput('MAGEData'))),
            fluidRow(box(uiOutput('USEventMap')))
    )
  )
)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(#title = "My Dashboard",
                                    #titleWidth = 350,
                                    title = HTML("<div style = 'background-color:black; vertical-align:middle'>
                    GEORGIA TECH COVID 19 DASHBOARD
                                 </div>"),
                                    titleWidth = "92%",
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                   textOutput("counter"),
                                                   icon("users")
                                                 ),
                                                 notificationItem(
                                                   text = "12 items delivered",
                                                   icon("truck"),
                                                   status = "success"
                                                 ),
                                                 notificationItem(
                                                   text = "Server load at 86%",
                                                   icon = icon("exclamation-triangle"),
                                                   status = "warning"
                                                 )
                                    )),
                    sidebar,
                    body
)

server <- function(input,output){
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  # OVERVIEW TABs
  positive_total <- reactive({overviewData[which(overviewData$county_resident == input$county),2]})
  death_total <- reactive({overviewData[which(overviewData$county_resident == input$county),3]})
  hospitalization_total <- reactive({overviewData[which(overviewData$county_resident == input$county),4]})
  death_ratio <- reactive({round(((overviewData[which(overviewData$county_resident == input$county),3])/(overviewData[which(overviewData$county_resident == input$county),2]))*100,2)})
  
  US_Confirmed_Total <- reactive({USdataTracking$positive[1]})
  US_Death_Total <- reactive({USdataTracking$death[1]})
  US_Hospitalization_Total <- reactive({USdataTracking$hospitalizedCumulative[1]})
  US_Death_ratio <- reactive({round((USdataTracking$death[1]/USdataTracking$positive[1])*100,2)})
  
  output$PositiveBox <- renderValueBox({
    valueBox(
      paste0(positive_total()), paste0(input$county," Positive Cases"), icon = icon("head-side-virus"), 
      color = 'red')
  })
  
  output$DeathBox <- renderValueBox({
    valueBox(
      paste0(death_total()), paste0(input$county," Deaths"),icon = icon("skull"), 
      color = 'purple')
  })
  
  output$HospitalizationBox <- renderValueBox({
    valueBox(
      paste0(hospitalization_total()), paste0(input$county," Hospitalizations"), icon = icon("procedures"), 
      color = 'blue')
  })
  
  output$DeathRatioBox <- renderValueBox({
    valueBox(
      paste0(death_ratio()),paste0(input$county," Death Ratio"), icon = icon("skull-crossbones"), 
      color = 'maroon')
  })
  output$TopTable <- renderDataTable(topTable, options = list(pageLength = 15, info = FALSE))
  output$dailyGrowth <- renderPlotly(fig)
  output$StateRank <- renderPlotly(state_ranking)
  
  output$USPositiveBox <- renderValueBox({
    valueBox(
      US_Confirmed_Total(), "USA Positive Cases", icon = icon("head-side-virus"),
      color = 'red')
  })
  output$USDeathBox <- renderValueBox({
    valueBox(
      US_Death_Total(), "USA Deaths",icon = icon("skull"), 
      color = 'purple')
  })
  output$USDeathRatioBox <- renderValueBox({
    valueBox(
      US_Death_ratio(),"USA Death Ratio", icon = icon("skull-crossbones"), 
      color = 'maroon')
  })
  output$USHospitalizationBox <- renderValueBox({
    valueBox(
      US_Hospitalization_Total(),"USA Hospitalizations", icon = icon("procedures"), 
      color = 'blue')
  })
  
  ##### MAPS TABS
  output$PositiveMap <- renderLeaflet(livelyMap('Positive','Confirmed Cases',"YlOrRd"))
  output$DeathMap <- renderLeaflet(livelyMap('DEATHS',"Death Cases", "YlGnBu"))
  output$HospitalizationMap <- renderLeaflet(livelyMap('HOSPITALIZATION',"Hospitalizations", "PuRd"))

  output$PositiveGIF <- renderImage({
    tags$video(src=paste("./GAConfirmedCasesMap.gif"),type="video/gif", width=100)
  })
  
  output$RiskyMap <- renderLeaflet({
    eventMap(input$number)}
    )

  ##### MAGE TABS
  output$SusceptPro <- renderPlotly(positive_plot)
  output$NCperday <- renderPlotly(NCperday_plot)
  output$DeathPro <- renderPlotly(death_plot)
  output$Dperday <- renderPlotly(Dperday_plot)
  output$Hospitalization <- renderPlotly(hospitalization_plot)
  output$ICU <- renderPlotly(ICU_plot)
  
  
  ##### FACTORS TABs
  output$Age <- renderPlotly(age_fig)
  output$Gender <- renderPlotly(sex_fig)
  output$Race <- renderggiraph(race_fig)
  output$Combined <- renderPlotly(factors_fig)
  
  #### Data Resources Tabs
  url1 <- a("Overview Data", href = 'https://covidtracking.com/api')
  url2 <- a("Today Covid-19 data", href = "https://dph.georgia.gov/covid-19-daily-status-report")
  url3 <- a("Data Tracking",href = "https://covid19-lake.s3.us-east-2.amazonaws.com/dashboard.html?dashboardName=COVID-19")
  url4 <- a("MAGE model created by Dr. Stephen Beckett & Dr. Joshua Weitz's Group",href = 'https://github.com/WeitzGroup/MAGEmodel_covid19_GA.git')
  url5 <- a("US Risky Score Map stimulated by Lee Seolha, School of City & Regional Plan, Georgia Tech", href = "https://covid19risk.biosci.gatech.edu/")
  output$OVData <- renderUI({tagList(url1)})
  output$todayData <- renderUI({tagList(url2)})
  output$dataTracking <- renderUI({tagList(url3)})
  output$MAGEData <- renderUI({tagList(url4)})
  output$USEventMap <- renderUI({tagList(url5)})
}

shinyApp(ui = ui, server = server)

