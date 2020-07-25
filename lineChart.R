library(ggplot2)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
library(ggthemes)
library(viridis)
#library(rgdal)
library(plotly)
library(data.table)

dataTracking = read.csv(url('https://covidtracking.com/api/v1/states/ga/daily.csv'))
#dataTracking = fread('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv')
dataTracking <- as.data.frame(dataTracking)
#dataTracking$REPORT_DATE <- as.Date(dataTracking$REPORT_DATE)
convert_date <- function(dataSet){
  year = substr(dataSet,start = 1, stop = 4)
  month = substr(dataSet,start = 5, stop = 6)
  day = substr(dataSet,start = 7, stop = 8)
  convertDate = as.Date(paste0(year,"-",month,"-",day))
  return(convertDate)
}

for (val in 1:length(dataTracking$date)){
  dataTracking$date[val] <- toString(convert_date(dataTracking$date[val]))
}

dataTracking$date <- as.Date(dataTracking$date)

### Get the neccesary columns in dataTracking: Date, Positive, hospitalizedCumulative,inICUcumulative, death
dataTracking <- dataTracking[,c("date","positiveIncrease","deathIncrease")]

max_Pos = max(dataTracking$positiveIncrease)
max_Dea = max(dataTracking$deathIncrease)

plotPD <- ggplot(dataTracking, aes(x = date)) +
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
  add_lines(x = ~(min(dataTracking$date) + 15):(max(dataTracking$date) + 15), y = ~dataTracking$deathIncrease, name = '', hoverinfo = 'skip' ,yaxis = 'y2', color = I("transparent"), showlegend = FALSE) %>%
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

fig
