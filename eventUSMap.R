library(magick)
library(ggplot2)
library(plotly)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
library(Hmisc)
#library(ggmap)
library(ggthemes)

##########################################################
dataTracking = read.csv('./dataInput/COVID-19-Activity.csv', as.is = TRUE)
#head(dataTracking)

USIndex <- which(dataTracking[,10] == 'United States', arr.ind = TRUE)
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
length(unique(USData$State))

databyState_today <- function(state){
  subData <- USData[which(USData[,"State"] == state),]
  subData <- subData[rev(order(subData$date)),]
  todayData <- subData[which(subData[,'date'] == subData$date[1]),]
  todayData <- todayData[order(todayData$County),]
  return(todayData)
}

databyState_twoWeeksago <- function(state){
  subData <- USData[which(USData[,"State"] == state),]
  subData <- subData[rev(order(subData$date)),]
  twoWeeksago <- subData$date[1] - 14
  pastData <- subData[which(subData[,'date'] == twoWeeksago),]
  pastData <- pastData[order(pastData$County),]
  return(pastData)
}
state_df_today <- databyState_today(USData$State[1])
state_df_past <- databyState_twoWeeksago(USData$State[1])


for (i in 2:length(unique(USData$State))){
  state_df_today <- rbind(state_df_today, databyState_today(unique(USData$State)[i]))
  state_df_past <- rbind(state_df_past, databyState_twoWeeksago(unique(USData$State)[i]))
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

#state_df$fips <- toString(state_df$fips)

df <- inner_join(state_df,unemp, by = 'fips')

riskyScoreMap <- function(g){
  risk <- as.data.frame(1-(1-10*(df$Today_Positive-df$Past_Positive)/df$pop)^g)
  df[,9] <- risk
  names(df)[9] <- 'risk'
  
  county <- map_data('county')
  final_df <- inner_join(df,county, by = c("County" = 'subregion'))
  ###############
  final_df$County <- capitalize(final_df$County)
  final_df$State <- capitalize(final_df$State)
  gg <- ggplot(data = county,mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = 'white', color = 'black')
  risky_map <- gg + geom_polygon( data=final_df, aes(x = long, y = lat, group = group, text = paste0('<b>County: </b>',County, "<br>", "<b>Risky Score: </b>",round(risk*100,2), "<br>",
                                                                                             "<b>Confirmed Cases: </b>", Today_Positive), fill = risk), 
                                         color="black", size = 0.2) +
    geom_blank(data = county, mapping = aes(x = long, y = lat))
  risky_map <- risky_map + scale_fill_continuous(low = 'lemonchiffon', high = 'firebrick', limits = c(0,max(final_df$risk)), 
                                                   breaks= quantile(final_df[,'risk'],c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1)),
                                                   na.value = "grey50") +
    coord_map("polyconic") + theme_map() +
    labs(title=paste0("Risky Score US County Level Map for a Big Event with ", g, ' Person(s)')) + theme(legend.position = "none") + 
    theme(plot.title = element_text(face = "bold")) + guides(fill = FALSE) +
    theme(plot.title = element_text(hjust=0.4))
  risky_map <- ggplotly(risky_map, tooltip = 'text') %>%
    highlight(
      "plotly_hover",
      selected = attrs_selected(line = list(color = "black"))
    ) 
  return(risky_map)
}
