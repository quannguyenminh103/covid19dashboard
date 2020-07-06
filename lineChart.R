library(ggplot2)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
library(ggthemes)
library(viridis)
#library(rgdal)
library(plotly)


#daily = read.csv('./dataInput/COVID-19-Activity.csv', as.is = TRUE)
daily = read.csv(url('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv'))

daily$COUNTY_NAME <- tolower(daily$COUNTY_NAME)
daily[which(daily == 'dekalb', arr.ind = TRUE)] <- 'de kalb'
georgiaIndex <- which(daily[,"PROVINCE_STATE_NAME"] == 'Georgia', arr.ind = TRUE)
georgiaDaily <- daily[georgiaIndex,]

georgiaDaily$REPORT_DATE<- strptime(as.character(georgiaDaily$REPORT_DATE), "%m/%d/%Y")
georgiaDaily$REPORT_DATE <- format(georgiaDaily$REPORT_DATE, "%Y-%m-%d")
georgiaDaily$REPORT_DATE <- as.Date(georgiaDaily$REPORT_DATE)
### Get Date Range
date_range <- seq(as.Date("2020-03-02"), Sys.Date()-2, "days")

count_byday <- function(dataSet, rdate){
  subset <- dataSet[which(dataSet[,"REPORT_DATE"] == rdate, arr.ind = TRUE),]
  positive_per_day = sum(subset["PEOPLE_POSITIVE_NEW_CASES_COUNT"])
  death_per_day = sum(subset["PEOPLE_DEATH_NEW_COUNT"])
  values <- list(positive_per_day,death_per_day)
  return(values)
}
statisticsTable <- function(dataSet){
  first_date <- count_byday(dataSet,"2020-03-01")
  date_df <- data.frame("DATE" = "2020-03-01","New_Positive_Cases" = unlist(first_date[1]),"New_Death_Cases" = unlist(first_date[2]))
  for (val in 2:length(date_range)){
    new_date <- count_byday(dataSet,toString(date_range[val]))
    date_df <- rbind(date_df, list(toString(date_range[val]),unlist(new_date[1]),unlist(new_date[2])))
  }
  date_df$DATE <- as.Date(date_df$DATE)
  return(date_df)
}
# get the Data Value Table:
georgiaData <- georgiaDaily[,c("REPORT_DATE","PEOPLE_POSITIVE_NEW_CASES_COUNT","PEOPLE_DEATH_NEW_COUNT")]

# filter Value Table with report date, new positive and death cases:
date_df <- statisticsTable(georgiaData)

max_Pos = max(date_df$New_Positive_Cases)
max_Dea = max(date_df$New_Death_Cases)

plotPD <- ggplot(date_df, aes(x = DATE)) +
  geom_col(aes( y = New_Positive_Cases, group = 1, text = paste("<b>Date: </b>", DATE, "<br>",
                                                                     "<b>New Confirmed Cases: </b>", New_Positive_Cases)), fill = "red") +
  geom_line(aes(y = New_Death_Cases/(max_Dea/max_Pos), group = 1, text = paste("<b>Date: </b>", DATE, "<br>",
                                                                                 "<b>New Death Cases: </b>", New_Death_Cases)),color = 'black') +
  scale_fill_manual('', labels = 'New Positive Cases', values = "#C00000") +
  scale_color_manual('', labels = 'New Death Cases', values = 'black') +
  labs(title = "Daily growth of Confirmed and Deaths by Date", y = "<b>New Positive Cases<b>") + guides(fill = FALSE) +
  theme_minimal() +  scale_y_continuous(sec.axis = sec_axis(~ . * (max_Dea/max_Pos),name = "New Death Cases"))


fig <- ggplotly(plotPD, tooltip = "text")  
# %>%
#    add_lines(data=date_df, x=~DATE, y=~New_Death_Cases, colors=NULL, yaxis="y2", 
#              inherit=FALSE, showlegend = FALSE) %>%
#    layout(yaxis2 = list(overlaying = "y", side = "right",
#                         tickfont = list(size=12), titlefont = list(size = 14), color = 'black',title = "New Death Cases"))
# fig

