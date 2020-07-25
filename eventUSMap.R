library(dplyr)
library(Hmisc)
library(stringr)
library(ggthemes)
library(data.table)
library(RColorBrewer)
library(leaflet)
library(sf)

dataTracking = fread('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv')
county <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/b96fa86886b1f7b9c62ed2853bd07c7bcdaa7f0a/COVID19-Event-Risk-Planner/map_data/tl_2017_us_county.geojson") 
stateline <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/b96fa86886b1f7b9c62ed2853bd07c7bcdaa7f0a/COVID19-Event-Risk-Planner/map_data/tl_2017_us_state.geojson")
pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/b96fa86886b1f7b9c62ed2853bd07c7bcdaa7f0a/COVID19-Event-Risk-Planner/map_data/county-population.csv", stringsAsFactors = FALSE)

##########################################################
dataTracking <- as.data.frame(dataTracking)
USIndex <- which(dataTracking[,10] == 'United States')

USdataTracking <- dataTracking[USIndex,]
USData <- USdataTracking[,c(2,4,3,8,1)]
# turn all counties into lowercase
USData <- USData%>% 
  rename(
    Positive = names(USData)[5],
    date = REPORT_DATE,
    County = COUNTY_NAME,
    State = PROVINCE_STATE_NAME,
    fips = COUNTY_FIPS_NUMBER
    
  )
USData$date<- strptime(as.character(USData$date), "%m/%d/%Y")
USData$date <- format(USData$date, "%Y-%m-%d")
USData$date <- as.Date(USData$date)

USData$County <- tolower(USData$County)
USData$State <- tolower(USData$State)

databyState <- function(state, num){
  subData <- USData[which(USData[,"State"] == state),]
  subData <- subData[rev(order(subData$date)),]
  dateSelected <- subData$date[1] - num
  todayData <- subData[which(subData[,'date'] == dateSelected),]
  todayData <- todayData[order(todayData$County),]
  return(todayData)
}

state_df_today <- databyState(USData$State[1],0)
state_df_past <- databyState(USData$State[1],14)

for (i in 2:length(unique(USData$State))){
  state_df_today <- rbind(state_df_today, databyState(unique(USData$State)[i],0))
  state_df_past <- rbind(state_df_past, databyState(unique(USData$State)[i],14))
}
state_df_today <- state_df_today[order(state_df_today$State),]
state_df_past <- state_df_past[order(state_df_past$State),]

state_df <- cbind(state_df_today, state_df_past[5])


state_df <- state_df %>%
  rename(
    Today_Positive = names(state_df)[5],
  )
state_df <- state_df %>%
  rename(
    Past_Positive = names(state_df)[6]
  )

df <- inner_join(state_df,pop, by = 'fips')

df$County <- capitalize(df$County)
df$State <- capitalize(df$State)

eventMap <- function(size){
  risk <- as.data.frame(round((1-(1-10*(df$Today_Positive-df$Past_Positive)/df$pop)**size)*100,2))
  df[,9] <- risk
  names(df)[9] <- 'risk'
  
  ###############
  df <- df[,c("County","State","date","fips","Today_Positive","risk")]
  
  df <- inner_join(county,df, by = c("GEOID" = "fips"))
  
  bins <- c(0,1,25,50,75,99,100)
  
  pal <- colorBin("YlOrRd", domain = df$risk, bins = bins, na.color = 'grey')
  
  labels <- sprintf(
    "<strong>County: %s</strong><br/>State: %s<br/>Risk Score: %g%%<br/>Confirmed Cases: %g",
    df$County, df$State, df$risk, df$Today_Positive
  ) %>% lapply(htmltools::HTML)
  labels_Missing <-sprintf(
    "<strong>County: %s</strong><br/>State: %s<br/>Risk Score: Missing Data",
    county$NAME, county$stname
  ) %>% lapply(htmltools::HTML)
  leaflet(df) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = county,
      fillColor = 'grey',
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
      label = labels_Missing,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
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
    addPolygons(
      data = stateline,
      fill = FALSE,
      color = 'black',
      weight = 1
    ) %>%
    addLegend(pal = pal, values = ~df$risk, opacity = 0.7, title = 'Risk Score (%)',
              position = "bottomright")
}
eventMap(100)