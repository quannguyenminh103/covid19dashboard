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
library(dplyr)
library(Hmisc)
library(stringr)
library(giphyr)
library(mapproj)
library(ggthemes)
library(plotly)
library(lubridate)
library(tidyverse)
library(viridis)
library(devtools)
library(png)
library(packcircles)
library(ggiraph)
library(tableHTML)
library(leaflet)
library(sf)
library(data.table)
library(RColorBrewer)
library(profvis)

overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)

USdataTracking = read.csv(url('https://covidtracking.com/api/v1/us/daily.csv'))
### OVERVIEW TABS:
overviewData <- rbind(data.frame(county_resident = "Georgia", t(colSums(overviewData[, -c(1,5)])), case_rate = mean(overviewData$case_rate)),overviewData)

county_list <- unique(overviewData$county_resident)


#### OVerview Tab:
## Table top 10:
GA_population = read.csv('./dataInput/POPsize.csv', as.is = TRUE)

for (i in 1:length(GA_population$County)){
  x <- unlist(strsplit(GA_population$County[i],','))[1]
  GA_population$County[i] <- str_replace_all(x,' County', '')
}

GA_population_updated <- GA_population[,c("County","POPSIZE")]
GA_population_updated <- GA_population_updated %>% add_row(County = c('Non-Georgia Resident','Unknown'), POPSIZE = c(0.00,0.00))

GA_population_updated <- GA_population_updated[order(GA_population_updated$County),]
index <- order(-overviewData$Positive)
topTable <- overviewData[index,]
### POSITIVE DENSITY:
first_county <- GA_population_updated$County[1]
density_df <- data.frame("County" = first_county,"Cases per 100K" = round((overviewData$Positive[1]/(GA_population_updated$POPSIZE[1]/100000)),2))

for (i in 2:length(GA_population_updated$County)){
  next_county <- GA_population_updated$County[i]
  density_df <- rbind(density_df, list(next_county, round((overviewData$Positive[i]/(GA_population_updated$POPSIZE[i]/100000)),2)))
}
inf_id <- which(density_df$Cases.per.100K == 'Inf')
for (i in 1:length(inf_id)){
  density_df$Cases.per.100K[inf_id[i]] <- 'NA'
}

topTable <- inner_join(topTable, density_df, by = c("county_resident" = 'County'))
names(topTable) <- c('County', 'Cumulative Confirmed', 'Cumulative Deaths', 'Cumulative Hospitalizations', 'Case Rate', 'Cases per 100K')

## Daily Growth Chart:
daily = read.csv(url('https://covidtracking.com/api/v1/states/ga/daily.csv'))
daily <- as.data.frame(daily)
convert_date <- function(dataSet){
  year = substr(dataSet,start = 1, stop = 4)
  month = substr(dataSet,start = 5, stop = 6)
  day = substr(dataSet,start = 7, stop = 8)
  convertDate = as.Date(paste0(year,"-",month,"-",day))
  return(convertDate)
}

for (val in 1:length(daily$date)){
  daily$date[val] <- toString(convert_date(daily$date[val]))
}

daily$date <- as.Date(daily$date)

### Get the neccesary columns in dataTracking: Date, Positive, hospitalizedCumulative,inICUcumulative, death
dataTracking <- daily[,c("date","positiveIncrease","deathIncrease")]

max_Pos = max(daily$positiveIncrease)
max_Dea = max(daily$deathIncrease)

plotPD <- ggplot(daily, aes(x = date)) +
  geom_col(aes( y = positiveIncrease, group = 1, text = paste("<b>Date: </b>", date, "<br>",
                                                              "<b>New Confirmed Cases: </b>", positiveIncrease)), fill = "red") +
  geom_line(aes(y = deathIncrease/(max_Dea/max_Pos), group = 1, text = paste("<b>Date: </b>", date, "<br>",
                                                                             "<b>New Death Cases: </b>", deathIncrease)),color = 'black') +
  scale_fill_manual('', labels = 'New Positive Cases', values = "#C00000") +
  scale_color_manual('', labels = 'New Death Cases', values = 'black') +
  labs(title = "Daily growth of Confirmed and Deaths by Date", x = 'Date', y = "New Positive Cases") + guides(fill = FALSE) +
  theme_minimal() +  scale_y_continuous(sec.axis = sec_axis(~ . * (max_Dea/max_Pos),name = "New Death Cases")) +
  theme(text=element_text(size=13,  family="georgia")) + theme(plot.margin = unit(c(1,1.4,1,1),"cm"))


fig <- ggplotly(plotPD, tooltip = "text") %>%
  add_lines(x = ~(min(daily$date) + 15):(max(daily$date) + 15), y = ~daily$deathIncrease, name = '', hoverinfo = 'skip' ,yaxis = 'y2', color = I("transparent"), showlegend = FALSE) %>%
  layout(
    yaxis2 = list(
      side = "right",
      overlaying = 'y',
      title = "New Death Cases",
      #showgrid = FALSE,
      titlefont = list(
        family = 'georgia',
        size = 17
      ),
      tickfont = list(
        size = 14
      )
    )
  )
### STATISTICS MAPs:
# get the data for Georgia Map
GAcounty <<- st_read('https://opendata.arcgis.com/datasets/dc20713282734a73abe990995de40497_68.geojson')

# read the CSV file
overviewData <- overviewData[order(overviewData$county_resident),]

GA_sf <- inner_join(GAcounty,overviewData, by = c("NAME10"  = "county_resident"))
GA_sf <- GA_sf%>%
  rename(
    County = NAME10,
    Death = DEATHS,
    Hospitalization = HOSPITALIZATION
  )

GA_sf <- GA_sf[order(GA_sf$County),]
livelyMap <- function(status,name,colorBands){
  bins <- quantile(overviewData[,status],c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1))
  pal <- colorBin(colorBands, domain = overviewData[,status], bins = bins)
  pal_label <- colorBin(colorBands, domain = overviewData[,status], n = 5)
  map <- leaflet(GA_sf) %>%
    addPolygons(
      data = GA_sf,
      fillColor = ~pal(overviewData[,status]),
      weight = 0.5,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = sprintf("<strong>County: %s</strong><br>%s: %g", overviewData$county_resident, name, overviewData[,status]) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend( # Legend options
      pal = pal_label, # Previously defined palette
      values = ~overviewData[,status], # Values from data frame 
      opacity = 0.7, # Opacity of legend
      title = NULL, # Title
      position = "bottomright"
    )
  map <- map %>%   fitBounds(-83, 35, -83, 30.5)
  return(map)
}

##### MAGE MODELS:
dailyData = read.csv(url('https://covidtracking.com/api/v1/states/ga/daily.csv'))
baseline = read.csv('./dataInput/Baseline_data.csv', as.is = TRUE)
transmission50 = read.csv('./dataInput/transmission50_data.csv', as.is = TRUE)
transmission75 = read.csv('./dataInput/transmission75_data.csv', as.is = TRUE)

population <- sum(GA_population$POPSIZE)

### Initially, date in this file has format Ymd (no "/), so we have to convert them into date form of R "Y-m-d"
convert_date <- function(dataSet){
  year = substr(dataSet,start = 1, stop = 4)
  month = substr(dataSet,start = 5, stop = 6)
  day = substr(dataSet,start = 7, stop = 8)
  convertDate = as.Date(paste0(year,"-",month,"-",day))
  return(convertDate)
}

for (val in 1:length(dailyData$date)){
  dailyData$date[val] <- toString(convert_date(dailyData$date[val]))
}

dailyData$date <- as.Date(dailyData$date)

### Get the neccesary columns in dailyData: Date, Positive, hospitalizedCumulative,inICUcumulative, death
dailyData <- dailyData[,c("date","positive","hospitalizedCumulative","inIcuCumulative","death","positiveIncrease","deathIncrease")]

renameData <- function(dataSet){
  dataSet <- dataSet[,c("X","S","R","D","Dperday","NCperday","Hcumulative","Ihcrit")]
  dataSet <- dataSet %>% 
    rename(
      date = names(dataSet)[1]
    )
  dataSet$date <- as.Date(dataSet$date)
  return(dataSet)
}
baseline <- renameData(baseline)

transmission50 <- renameData(transmission50)
transmission75 <- renameData(transmission75)

### ---PC : Past & Present / --- Future: Predicted Future data
#### BASELINE TRANSFORMATIONS inner join with actual data for easier to draw the graphs:
baselinePP <- inner_join(dailyData,baseline[which(baseline$date <= Sys.Date()),], by = "date")
baselineFuture <- baseline[which(baseline$date > Sys.Date()),]
#### 50% Transmissions Reduction Data TRANSFORMATIONS inner join with actual data for easier to draw the graphs:
transmission50PP <- inner_join(dailyData,transmission50[which(transmission50$date <= Sys.Date()),], by = "date")
transmission50Future <- transmission50[which(transmission50$date > Sys.Date()),]
#### 75% Transmissions Reduction Data TRANSFORMATIONS inner join with actual data for easier to draw the graphs:
transmission75PP <- inner_join(dailyData,transmission75[which(transmission75$date <= Sys.Date()),], by = "date")
transmission75Future <- transmission75[which(transmission75$date > Sys.Date()),]

plotGraph <- function(name,actual, compare, yaxis,pop, n,limit){
  plotChart <- ggplot() + 
    geom_line(aes(x = baselinePP$date, y = abs(pop-baselinePP[,compare])/n, fill = "Baseline Past & Present Data"), size = 1, color = 'red', show.legend = TRUE) +
    geom_point(aes(x = baselinePP$date, y =  abs(pop-baselinePP[,compare])/n, fill = "Baseline Past & Present Data", text = paste0('<b>Date: </b>',baselinePP$date,'<br>','<b>MAGE Baseline Calculated Values: </b>', abs(pop-baselinePP[,compare])/n)), shape = 16, size =1.5, color = 'red')+
    
    geom_line(aes(x = baselineFuture$date, y =  abs(pop-baselineFuture[,compare])/n, fill = "Baseline Future Predicted Data"), size = 1, color = 'red', linetype = 'dotdash') +
    geom_point(aes(x = baselineFuture$date, y =  abs(pop-baselineFuture[,compare])/n, fill = "Baseline Future Predicted Data", text = paste0('<b>Date: </b>',baselineFuture$date,'<br>','<b>MAGE Baseline Calculated Values: </b>', abs(pop-baselineFuture[,compare])/n)), shape = 5, size =1.5, color = 'red') +
    
    geom_line(aes(x = transmission50PP$date, y =  abs(pop-transmission50PP[,compare])/n, fill = "50% TR Past & Present Data"), size = 1, color = 'orange') +
    #geom_point(aes(x = transmission50PP$date, y =  abs(pop-transmission50PP[,compare])/n, fill = "50% TR Past & Present Data", text = paste0('<b>Date: </b>',transmission50PP$date,'<br>','<b>MAGE 50% TR Calculated Values: </b>', abs(pop-transmission50PP[,compare])/n)), shape = 16, size =1.5, color = 'orange') +
    
    geom_line(aes(x = transmission50Future$date, y =  abs(pop-transmission50Future[,compare])/n, fill = "50% TR Future Predicted Data"), size = 1, color = 'orange', linetype = 'dotdash') +
    geom_point(aes(x = transmission50Future$date, y =  abs(pop-transmission50Future[,compare])/n, fill = "50% TR Future Predicted Data", text = paste0('<b>Date: </b>',transmission50Future$date,'<br>','<b>MAGE 50% TR Calculated Values: </b>', abs(pop-transmission50Future[,compare])/n)), shape = 5, size =1.5, colour = 'orange') +
    
    geom_line(aes(x = transmission75PP$date, y =  abs(pop-transmission75PP[,compare])/n, fill = "50% TR Past & Present Data"), size = 1, color = 'green') +
    #geom_point(aes(x = transmission75PP$date, y =  abs(pop-transmission75PP[,compare]/n), fill = "50% TR Past & Present Data", text = paste0('<b>Date: </b>',transmission75PP$date,'<br>','<b>MAGE 75% TR Calculated Values: </b>',transmission75PP[,compare])), shape = 16, size =1.5, color = 'green') +
    
    geom_line(aes(x = transmission75Future$date, y =  abs(pop-transmission75Future[,compare])/n, fill = "75% TR Future Predicted Data"), size = 1, color = 'green', linetype = 'dotdash') +
    geom_point(aes(x = transmission75Future$date, y =  abs(pop-transmission75Future[,compare])/n, fill = "75% TR Future Predicted Data", text = paste0('<b>Date: </b>',transmission75Future$date,'<br>','<b>MAGE 75% TR Calculated Values: </b>', abs(pop-transmission75Future[,compare])/n)), shape = 5, size =1.5, color = 'green') +
    
    #geom_line(aes(x = baselinePP$date, y = baselinePP[,actual], fill = "Actual Data"), size = 1, color = 'purple') +
    geom_point(aes(x = baselinePP$date, y = baselinePP[,actual], fill = "Actual Data", text = paste0('<b>Date: </b>',baselinePP$date,'<br>','<b>Actual Values: </b>',baselinePP[,actual])), shape = 15, size =1.5, color = 'purple') +
    
    guides(fill=FALSE)+
    labs(title = paste("Daily growth of", name,"by Date using MAGE model"), y = yaxis, x = "Date") + ylim(0,max(baseline[,compare])/limit) +
    #scale_x_date(breaks  = as.Date(c("2020-04-28","2020-05-01", "2020-06-01", "2020-07-01", "2020-07-04")))
    scale_x_date(date_breaks = "1 month", date_labels =  "%d %b", minor_breaks = as.Date(c("2020-04-28","2020-05-01","2020-07-04","2020-08-17"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  figure <- ggplotly(plotChart, tooltip = 'text')
  return(figure)
}

# #### Event Map:
dataTracking = fread('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv')
dataTracking <- as.data.frame(dataTracking)
dataTracking$REPORT_DATE <- as.Date(dataTracking$REPORT_DATE)
GAIndex <- which(dataTracking[,'PROVINCE_STATE_NAME'] == 'Georgia')
GAdataTracking <- dataTracking[GAIndex,]
GAData <- GAdataTracking[,c('COUNTY_NAME',"REPORT_DATE","PEOPLE_POSITIVE_CASES_COUNT")]
GAData <- GAData%>% 
  rename(
    Positive = names(GAData)[1],
    date = REPORT_DATE,
    County = COUNTY_NAME
  )
countyList <- unique(GAData$County)
countyList <- countyList[order(countyList)]
databyState <- function(county, num){
  subData <- GAData[which(GAData[,"County"] == county),]
  subData <- subData[rev(order(subData$date)),]
  dateSelected <- subData$date[1] - num
  todayData <- subData[which(subData[,'date'] == dateSelected),]
  todayData <- todayData[order(todayData$County),]
  return(todayData)
}
df_today <- databyState(countyList[1],0)
df_past <- databyState(countyList[1],14)
for (i in 2:length(countyList)){
  df_today <- rbind(df_today, databyState(countyList[i],0))
  df_past <- rbind(df_past, databyState(countyList[i],14))
}
df_today <- df_today[order(df_today$County),]
df_past <- df_past[order(df_past$County),]
df <- cbind(df_today, df_past[3])
df <- df %>%
  rename(
    Today_Positive = names(df)[3]
  )
df <- df %>%
  rename(
    Past_Positive = names(df)[4]
  )
final_df <- inner_join(df, GA_population_updated, by = "County")
eventMap <- function(size){
  risk <- as.data.frame(round((1-(1-10*(final_df$Today_Positive-final_df$Past_Positive)/final_df$POPSIZE)**size)*100,2))
  final_df[,5] <- risk
  names(final_df)[5] <- 'risk'
  ###############
  final_df <- final_df[,c("County","date","Today_Positive","Past_Positive","risk")]
  GAmap <- inner_join(GAcounty,final_df, by = c("NAME10" = "County"))
  GAmap <- GAmap[order(GAmap$NAME10),]
  GAmap$riskLabels <- GAmap$risk
  GAmap <- GAmap %>%
    mutate(riskLabels = case_when(
      riskLabels == 100 ~ '> 99',
      riskLabels == 0 ~ '< 1',
      is.na(riskLabels) ~ 'No data',
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0,1,25,50,75,99,100)
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99")
  pal <- colorBin("YlOrRd", domain = GAmap$risk, bins = bins, na.color = 'grey')
  labels <- sprintf(
    "<strong>County: %s</strong><br/>Risk Score: %s%%<br/>Recorded Cases in the last 2 weeks: %g",
    GAmap$NAME10, GAmap$riskLabels, (GAmap$Today_Positive - GAmap$Past_Positive)
  ) %>% lapply(htmltools::HTML)
  leaflet(GAmap) %>%
    fitBounds(-83, 35, -83, 30.5) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(risk),
      weight = 0.5,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~GAmap$risk, opacity = 0.7, title = 'Risk Score (%)',
              position = "bottomright",labFormat = function(type, cuts, p) {
                paste0(legendlabs)
              })
}

##### DEMOGRAPHICS TABS:
demographicsData = read.csv('./dataInput/deaths.csv', as.is = TRUE)

# there is a '.' and '90+' in the files so we have to convert them into readible variables.
for (i in 1:length(demographicsData$age)){
  if (demographicsData$age[i] == '.'){
    demographicsData$age[i] <- 'Unknown'
  } else if (demographicsData$age[i] == '90+'){ # will just change 90+ to 90 temporarily for easier to caluclate, then
    # convert later into 90+
    demographicsData$age[i] <- '90'
  }
}
# turn all column into numeric for comparisons later
demographicsData$age <- as.numeric(demographicsData$age)
# get a short name for column 'age'
age <- demographicsData$age
for (i in 1:length(age)){
  if (!is.na(age[i])){
    if (age[i] >= 0 && age[i] < 20){
      age[i] <- '0-19'
    } else if (age[i] >= 20 && age[i] < 30){
      age[i] <- '20-29'
    } else if (age[i] >= 30 && age[i] < 40){
      age[i] <- '30-39'
    } else if (age[i] >= 40 && age[i] < 50){
      age[i] <- '40-49'
    } else if (age[i] >= 50 && age[i] < 60){
      age[i] <- '50-59'
    } else if (age[i] >= 60 && age[i] < 70){
      age[i] <- '60-69'
    } else if (age[i] >= 70 && age[i] < 80){
      age[i] <- '70-79'
    } else if (age[i] >= 80 && age[i] < 90){
      age[i] <- '80-89'
    } else if (age[i] >= 90){
      age[i] <- '90+'
    }
  } else{
    age[i] <- 'Unknown'
  }
}
# Convert back into the data frame
demographicsData$age <- age

### Set up the font
f <- list(
  family = "Georgia, monospace",
  size = 18,
  color = "#7f7f7f"
)
y <- list(
  title = "Number of Deaths",
  titlefont = f
)

# Count how many deaths in each group of age

## AGE
ageFreq <- table(demographicsData$age)
ageFreq <- as.data.frame(ageFreq)
names(ageFreq)[1] <- 'Age'

x_age <- list(
  title = 'Age',
  titlefont = f
)

age_fig <- plot_ly(ageFreq, x = ~Age, y = ~Freq, type = 'bar', color = I('orange'),hoverinfo = 'text',
                   text = ~paste0('<b>Age: </b>',Age,'<br>','<b>Number of Deaths: </b>',Freq))
age_fig <- age_fig %>% layout(
  title = 'Number of Deaths by Age',
  xaxis = x_age,
  yaxis = y
) %>%
  add_text(text=ageFreq$Freq, textposition = 'top', showlegend = FALSE,
           textfont=list(size=15, color="black"))

# SEX
sexFreq <- table(demographicsData$sex)
sexFreq <- as.data.frame(sexFreq)
names(sexFreq)[1] <- 'Sex'

x_sex <- list(
  title = 'Gender',
  titlefont = f
)

sex_fig <- plot_ly(sexFreq, x = ~Sex, y = ~Freq, type = 'bar', color = I('magenta'),hoverinfo = 'text',
                   text = ~paste0('<b>Gender: </b>',Sex,'<br>','<b>Number of Deaths: </b>',Freq))
sex_fig <- sex_fig %>% layout(
  title = 'Number of Deaths by Gender',
  xaxis = x_sex,
  yaxis = y
) %>%
  add_text(text=sexFreq$Freq, textposition = 'top', showlegend = FALSE,
           textfont=list(size=15, color="black"))

# RACE
raceFreq <- table(demographicsData$race)
raceFreq <- as.data.frame(raceFreq)
names(raceFreq)[1] <- 'Race'

# Generate the layout
packing <- circleProgressiveLayout(raceFreq$Freq, sizetype='area')
raceFreq <- cbind(raceFreq, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
# Make the plot with a few differences compared to the static version:
race_plot <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = paste0(raceFreq$Race[id],'<br>',"<b>Number of Deaths: </b>",raceFreq$Freq[id]), data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  #geom_text(data = raceFreq, aes(x, y, label = gsub("Race", "", Race)), size=5, color="black") +
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  labs(title = 'Number of Deaths by Race') + guides(fill = FALSE) +
  coord_equal() + theme_void() +
  theme(plot.title = element_text(face = "bold")) + theme(plot.title = element_text(hjust=0.4)) 
# Turn it interactive
race_fig <- ggiraph(ggobj = race_plot, width_svg = 7, height_svg = 7)

library(plyr)
### Factors Analysis
factors <- demographicsData[,c('age','race','sex')]
factors_analysis <- count(factors, vars = c('age','race','sex'))
unique(factors_analysis$race)
factors_plot <- ggplot(factors_analysis, aes(x = factor(race, levels = c("African-American/ Black","American Indian/ Alaska Native","Asian","Native Hawaiian/ Pacific Islander","White","Other","Unknown")),
                                                        y = age, size = freq, fill = sex)) +
  geom_point(shape = 21, aes(text = paste('<b>Number of Deaths: </b>',freq))) + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle('Distribution of COVID-19 Deaths in GA by Race, Age & Sex') +
  scale_fill_manual(values = c('violetred1','steelblue','purple')) +
  labs(x = 'Race', y = 'Age') +  scale_size(range = c(3,10)) + 
  theme(axis.text.x = element_text(size = 8))  + guides(fill = FALSE, size = FALSE)

factors_fig <- ggplotly(factors_plot, tooltip = 'text', height = 520) %>%
  layout(autosize=TRUE)

###  ASSEMBLE THE WEBSITE CONTENTs
# 
# sidebar <- dashboardSidebar(width = 300,
#   sidebarMenu(
#     menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
#     #menuItem("Statistics Maps", icon = icon("map"), tabName = "maps",
#      #        badgeLabel = "Time", badgeColor = "green"),
#     #menuItem("MAGE Model Charts", icon = icon("chart-line"), tabName = "mage",
#       #       badgeLabel = "Highlight", badgeColor = "green"),
#     #menuItem("COVID-19 by Age, Race & Gender", icon = icon("layer-group"), tabName = 'factors'),
#     menuItem("Data Resources", icon = icon("file-code-o"), tabName = 'data')
#   )
# )

body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 15
  ),
    
  fluidRow(column(12,
                  fluidRow(
                    strong(h1('COVID-19 in Georgia', align = 'center')),
                    column(4,selectInput("county", "Choose a county: ",
                                         county_list))
                  ),
                  fluidRow( class ='text-center',
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
                            
                            
                  ))
  ),
  fluidRow(column(12,
                  strong(h1('Current COVID-19 Event Risk for Georgia', align = 'center')),
                  #fluidRow(
                  #  #box(width = 6,leafletOutput('PositiveMap')),
                  #box(width = 6,leafletOutput('DeathMap')),
                  #box(width = 6, leafletOutput('HospitalizationMap'))
                  #),
                  fluidRow(
                    box(width = 3, height = 610, 
                        p('This map shows the risk level of attending an event, given the event size and location (assuming 10:1 ascertainment bias'),
                        br(),
                        p('The risk level is the estimated chance (0-100%) that at least 1 COVID-19 positive individual will be present at an event in a county, given the size of the event.'),
                        br(),
                        p('Choose an event size. Use the drop-down menu to choose a county you would like to zoom in on.'),
                        br(),
                        numericInput('number',"Event Size",25,min = 1), align = 'center'),
                    box(width = 9, leafletOutput('RiskyMap', height = '600px'), align = 'center', height = 610)
                  ))),
  fluidRow(column(12,
                  strong(h1('Daily Updates'), align = 'center')),
                  fluidRow(class = 'text-center',
                           box(dataTableOutput('TopTable')),
                           box(plotlyOutput('dailyGrowth', height = 350)),
                           #box(plotlyOutput('StateRank', height = 350))
                           box(width = 6, img(src="GAConfirmedCasesMap.gif",height = "390px"), align = 'center', height = 430)         
                  )),
  fluidRow(column(12,
                  strong(h1('MAGE Models',align = 'center')),
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
  )),
  fluidRow(column(12,
                  strong(h1('COVID-19 Deaths by Age, Gender & Race', align = 'center')),
                  fluidRow(
                    box(plotlyOutput('Age')),
                    box(plotlyOutput('Gender'))
                  ),
                  fluidRow(
                    box(ggiraphOutput("Race")),
                    box(plotlyOutput('Combined'), height = 530)
                  )
  )),
  fluidRow(column(12,
                  strong(h1('Data Resources', align = 'center')),
                  tags$style(make_css(list('.box', 
                                           c('font-size', 'font-family', 'color'), 
                                           c('16px', 'georgia', 'black')))),
                  box(width =12, align = 'center',
                    p(uiOutput("todayData")),
                    p(uiOutput('GAEventMap')),
                    p(uiOutput('OVData')),
                    p(uiOutput('MAGEData')),
                    p(uiOutput('dataTracking')),
                    p(uiOutput('code'))
  ))),
  fluidRow(column(12,
                 strong(h1('Website and Dashboard Development', align = 'center')),
                 box(width = 12, align = 'center',
                     p('Quan (King) Minh Nguyen: qnguyen83@gatech.edu'),
                     p('Stephen Beckett: stephen.beckett@biology.gatech.edu'),
                     p('Joshua Weitz: jsweitz@gatech.edu')
                     )
                 ))
)



ui <- dashboardPage(skin = "red",
                    dashboardHeader(#title = "My Dashboard",
                                    #titleWidth = 350,
                                    title = HTML("<div style = 'background-color:black; vertical-align:middle'>
                    GEORGIA TECH COVID 19 DASHBOARD
                                 </div>"),
                                    titleWidth = "95%",
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
                    #sidebar,
                    dashboardSidebar(disable = TRUE),
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
      paste0(death_ratio()),paste0(input$county," Mortality (%)"), icon = icon("skull-crossbones"), 
      color = 'maroon')
  })
  output$TopTable <- renderDataTable(topTable, options = list(pageLength = 15, info = FALSE))
  output$dailyGrowth <- renderPlotly(fig)
  #output$StateRank <- renderPlotly(state_ranking)
  
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
      US_Death_ratio(),"USA Mortality (%)", icon = icon("skull-crossbones"), 
      color = 'maroon')
  })
  output$USHospitalizationBox <- renderValueBox({
    valueBox(
      US_Hospitalization_Total(),"USA Hospitalizations", icon = icon("procedures"), 
      color = 'blue')
  })
  
  ##### MAPS TABS
  #output$PositiveMap <- renderLeaflet(livelyMap('Positive','Confirmed Cases',"YlOrRd"))
  #output$DeathMap <- renderLeaflet(livelyMap('DEATHS',"Death Cases", "YlGnBu"))
  #output$HospitalizationMap <- renderLeaflet(livelyMap('HOSPITALIZATION',"Hospitalizations", "PuRd"))
  # 
# 
  output$RiskyMap <- renderLeaflet({
   eventMap(input$number)}
   )

  ##### MAGE TABS
  output$SusceptPro <- renderPlotly(plotGraph("Confirmed","positive","S","Cumulative Confirmed Cases",population,5,80))
  output$NCperday <- renderPlotly(plotGraph("New Cases","positiveIncrease",'NCperday',"New Cases per Day",0,1,45))
  output$DeathPro <- renderPlotly(plotGraph("Deaths","death","D","Cumulative Recorded Deaths",0,1,6))
  output$Dperday <- renderPlotly(plotGraph("New Deaths","deathIncrease","Dperday","New Deaths per Day",0,1,6))
  output$Hospitalization <- renderPlotly(plotGraph("Hospitalizations","hospitalizedCumulative","Hcumulative","Cumulative Recorded Hospitalizations",0,1,6))
  output$ICU <- renderPlotly(plotGraph("ICU Cases","inIcuCumulative","Ihcrit","Cumulative ICU Cases",0,1,6))

  ##### FACTORS TABs
  output$Age <- renderPlotly(age_fig)
  output$Gender <- renderPlotly(sex_fig)
  output$Race <- renderggiraph(race_fig)
  output$Combined <- renderPlotly(factors_fig)
  
  #### Data Resources Tabs
  url1 <- a("COVID-19 Georgia Data by County (including Age, Race, & Gender)", href = "https://dph.georgia.gov/covid-19-daily-status-report")
  url2 <- a("Daily COVID-19 Updates Data in Georgia", href = 'https://covidtracking.com/api')
  url3 <- a("Time Series COVID 19 Data Tracking",href = "https://covid19-lake.s3.us-east-2.amazonaws.com/dashboard.html?dashboardName=COVID-19")
  url4 <- a("MAGE model created by Dr. Stephen Beckett & Dr. Joshua Weitz's Group",href = 'https://github.com/WeitzGroup/MAGEmodel_covid19_GA.git')
  url5 <- a("US Risky Score Map stimulated by Lee Seolha, School of City & Regional Plan, Georgia Tech", href = "https://covid19risk.biosci.gatech.edu/")
  url6 <- a('Github Codes for the Dashboard', href = 'https://github.com/quannguyenminh103/covid19dashboard')
  output$OVData <- renderUI({tagList(url2)})
  output$todayData <- renderUI({tagList(url1)})
  output$dataTracking <- renderUI({tagList(url3)})
  output$MAGEData <- renderUI({tagList(url4)})
  output$GAEventMap <- renderUI({tagList(url5)})
  output$code <- renderUI({tagList(url6)})
}

shinyApp(ui = ui, server = server)

