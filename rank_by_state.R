library(tidyverse)
library(dplyr)

dataTracking = read.csv('./dataInput/COVID-19-Activity.csv', as.is = TRUE)

USIndex <- which(dataTracking[,"COUNTRY_SHORT_NAME"] == 'United States', arr.ind = TRUE)
USdataTracking <- dataTracking[USIndex,]
USData <- USdataTracking[,c(3,4,12,7)]
# turn all counties into lowercase

USData <- USData%>% 
  rename(
    Positive = PEOPLE_POSITIVE_NEW_CASES_COUNT,
    date = REPORT_DATE,
    State = PROVINCE_STATE_NAME,
    Death = PEOPLE_DEATH_NEW_COUNT,
  )
state_list <- unique(USData$State)[order(unique(USData$State))]

USData$date<- strptime(as.character(USData$date), "%m/%d/%Y")
USData$date <- format(USData$date, "%Y-%m-%d")
USData$date <- as.Date(USData$date)
### Get Date Range

count_bystate <- function(dataSet, state){
  subset <- dataSet[which(dataSet[,"State"] == state, arr.ind = TRUE),]
  positive_by_state = sum(subset["Positive"])
  death_by_state = sum(subset["Death"])
  values <- list(positive_by_state,death_by_state)
  return(values)
}


statisticsTable <- function(dataSet){
  first_state <- count_bystate(dataSet,state_list[1])
  state_df <- data.frame("State" = state_list[1],"Total_Positive_Cases" = unlist(first_state[1]),"Total_Death_Cases" = unlist(first_state[2]))
  for (val in 2:length(state_list)){
    new_state <- count_bystate(dataSet,state_list[val])
    state_df <- rbind(state_df,list(state_list[val],unlist(new_state[1]),unlist(new_state[2])))
  }
  return(state_df)
}

# get the Data Value Table:
StateData <- statisticsTable(USData)

US_total_Positive <- sum(StateData$Total_Positive_Cases)
US_total_Death <- sum(StateData$Total_Death_Cases)

USindex <- order(-StateData$Total_Positive_Cases)
topStateTable <- StateData[USindex,]
top10State <- topStateTable[1:10,]

category <- names(top10State)

df2 <- rbind(
  data.frame(top10State$State, "count" = top10State$Total_Positive_Cases, "type"="Positive"),
  data.frame(top10State$State, "count" = top10State$Total_Death_Cases, "type"="Death")
)

state_bar <- ggplot(df2, aes( fill = type,x = factor(top10State.State, levels = c(unique(top10State.State))), y = count)) +
  geom_bar(aes(text = paste("<b>State: </b>",top10State.State, "<br>","<b>Total Confirmed Cases: </b>", top10State$Total_Positive_Cases, '<br>',
                            "<b>Total Deaths: </b>", top10State$Total_Death_Cases)),stat='identity') +
  scale_fill_manual("Condition", values = alpha( c("firebrick", "dodgerblue4"), 1) )  +
  labs(title = "Top 10 States by Confirmed and Deaths", y = "Total Cases", x = '<b>State<b>') +
  theme_minimal() + coord_flip()

state_ranking <- ggplotly(state_bar, tooltip = "text")
state_ranking

### NEW PROJECTION
# head(USData)
# 
# count_bystate_day <- function(dataSet, state, rdate){
#   subData <- dataSet[which(dataSet[,"State"] == state, arr.ind = TRUE),]
#   subset <- subData[which(subData[,"date"] == rdate, arr.ind = TRUE),]
#   positive_by_state_perD = sum(subset["Positive"])
#   return(positive_by_state_perD)
# }
# state_list <- unique(USData$State)
# day1 <- count_bystate_day(USData, state_list[1],as.Date('2020-07-01'))
# day1
# statisticsTable_day <- function(dataSet, rdate){
#   first_state <- count_bystate(dataSet,state_list[1])
#   state_df <- data.frame("State" = state_list[1],"Total_Positive_Cases" = unlist(first_state[1]),"Total_Death_Cases" = unlist(first_state[2]))
#   for (val in 2:length(state_list)){
#     new_state <- count_bystate(dataSet,state_list[val])
#     state_df <- rbind(state_df,list(state_list[val],unlist(new_state[1]),unlist(new_state[2])))
#   }
#   return(state_df)
# }
# 
