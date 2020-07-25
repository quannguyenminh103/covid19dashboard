library(dplyr)
library(Hmisc)
library(stringr)
library(ggthemes)
library(data.table)
library(RColorBrewer)
library(leaflet)
library(sf)
library(mapview)

dataTracking = fread('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv')
GAcounty <<- st_read('https://opendata.arcgis.com/datasets/dc20713282734a73abe990995de40497_68.geojson')
GA_population = read.csv('./dataInput/POPsize.csv', as.is = TRUE)
for (i in 1:length(GA_population$County)){
  x <- unlist(strsplit(GA_population$County[i],','))[1]
  GA_population$County[i] <- str_replace_all(x,' County', '')
}

GA_population_updated <- GA_population[,c("County","POPSIZE")]
GA_population_updated <- GA_population_updated %>% add_row(County = c('Non-Georgia Resident','Unknown'), POPSIZE = c(0.00,0.00))

GA_population_updated <- GA_population_updated[order(GA_population_updated$County),]
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
    #addControl('Georgia Cases', position = "topleft", className="map-title") %>%
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
