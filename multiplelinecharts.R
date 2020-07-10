# library(ggplot2)
# library(maps)
# #library(plyr)
# library(dplyr)
# library(tidyverse)
# library(Hmisc)
# library(stringr)
# library(ggthemes)
# library(viridis)
# #library(rgdal)
# library(plotly)


dailyData = read.csv(url('https://covidtracking.com/api/v1/states/ga/daily.csv'))
baseline = read.csv('./dataInput/Baseline_data.csv', as.is = TRUE)
transmission50 = read.csv('./dataInput/transmission50_data.csv', as.is = TRUE)
transmission75 = read.csv('./dataInput/transmission75_data.csv', as.is = TRUE)
POP = read.csv('./dataInput/POPsize.csv', as.is = TRUE)

population <- sum(POP$POPSIZE)

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
#dailyData <- dailyData[,c(1,3,7,9,17,27,33)]
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

plotGraph <- function(name,actual, compare, yaxis,pop){
  plotChart <- ggplot() + 
    geom_line(aes(x = baselinePP$date, y = abs(pop-baselinePP[,compare]), fill = "Baseline Past & Present Data"), size = 1, color = 'red', show.legend = TRUE) +
    geom_point(aes(x = baselinePP$date, y =  abs(pop-baselinePP[,compare]), fill = "Baseline Past & Present Data", text = paste0('<b>Date: </b>',baselinePP$date,'<br>','<b>MAGE Baseline Calculated Values: </b>', abs(pop-baselinePP[,compare]))), shape = 16, size =1.5, color = 'red')+
    
    geom_line(aes(x = baselineFuture$date, y =  abs(pop-baselineFuture[,compare]), fill = "Baseline Future Predicted Data"), size = 1, color = 'red', linetype = 'dotdash') +
    geom_point(aes(x = baselineFuture$date, y =  abs(pop-baselineFuture[,compare]), fill = "Baseline Future Predicted Data", text = paste0('<b>Date: </b>',baselineFuture$date,'<br>','<b>MAGE Baseline Calculated Values: </b>', abs(pop-baselineFuture[,compare]))), shape = 5, size =1.5, color = 'red') +

    geom_line(aes(x = transmission50PP$date, y =  abs(pop-transmission50PP[,compare]), fill = "50% TR Past & Present Data"), size = 1, color = 'orange') +
    #geom_point(aes(x = transmission50PP$date, y =  abs(pop-transmission50PP[,compare]), fill = "50% TR Past & Present Data", text = paste0('<b>Date: </b>',transmission50PP$date,'<br>','<b>MAGE 50% TR Calculated Values: </b>', abs(pop-transmission50PP[,compare]))), shape = 16, size =1.5, color = 'orange') +
    
    geom_line(aes(x = transmission50Future$date, y =  abs(pop-transmission50Future[,compare]), fill = "50% TR Future Predicted Data"), size = 1, color = 'orange', linetype = 'dotdash') +
    geom_point(aes(x = transmission50Future$date, y =  abs(pop-transmission50Future[,compare]), fill = "50% TR Future Predicted Data", text = paste0('<b>Date: </b>',transmission50Future$date,'<br>','<b>MAGE 50% TR Calculated Values: </b>', abs(pop-transmission50Future[,compare]))), shape = 5, size =1.5, colour = 'orange') +
    
    geom_line(aes(x = transmission75PP$date, y =  abs(pop-transmission75PP[,compare]), fill = "50% TR Past & Present Data"), size = 1, color = 'green') +
    #geom_point(aes(x = transmission75PP$date, y =  abs(pop-transmission75PP[,compare]), fill = "50% TR Past & Present Data", text = paste0('<b>Date: </b>',transmission75PP$date,'<br>','<b>MAGE 75% TR Calculated Values: </b>',transmission75PP[,compare])), shape = 16, size =1.5, color = 'green') +
    
    geom_line(aes(x = transmission75Future$date, y =  abs(pop-transmission75Future[,compare]), fill = "75% TR Future Predicted Data"), size = 1, color = 'green', linetype = 'dotdash') +
    geom_point(aes(x = transmission75Future$date, y =  abs(pop-transmission75Future[,compare]), fill = "75% TR Future Predicted Data", text = paste0('<b>Date: </b>',transmission75Future$date,'<br>','<b>MAGE 75% TR Calculated Values: </b>', abs(pop-transmission75Future[,compare]))), shape = 5, size =1.5, color = 'green') +
    
    #geom_line(aes(x = baselinePP$date, y = baselinePP[,actual], fill = "Actual Data"), size = 1, color = 'purple') +
    geom_point(aes(x = baselinePP$date, y = baselinePP[,actual], fill = "Actual Data", text = paste0('<b>Date: </b>',baselinePP$date,'<br>','<b>Actual Values: </b>',baselinePP[,actual])), shape = 15, size =1.5, color = 'purple') +
    
    guides(fill=FALSE)+
    labs(title = paste("Daily growth of", name,"by Date using MAGE model"), y = yaxis, x = "Date") + ylim(0,max(baseline[,compare])/4) +
    #scale_x_date(breaks  = as.Date(c("2020-04-28","2020-05-01", "2020-06-01", "2020-07-01", "2020-07-04")))
    scale_x_date(date_breaks = "1 month", date_labels =  "%d %b", minor_breaks = as.Date(c("2020-04-28","2020-05-01","2020-07-04","2020-08-17"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  figure <- ggplotly(plotChart, tooltip = 'text')
  return(figure)
}
positive_plot <- plotGraph("Confirmed","positive","S","Cumulative Confirmed Cases",population)
positive_plot

death_plot <- plotGraph("Deaths","death","D","Cumulative Recorded Deaths",0)
death_plot

hospitalization_plot <- plotGraph("Hospitalizations","hospitalizedCumulative","Hcumulative","Cumulative Recorded Hospitalizations",0)
hospitalization_plot
head(baselinePP)

NCperday_plot <- plotGraph("New Cases","positiveIncrease",'NCperday',"New Cases per Day",0)
NCperday_plot

Dperday_plot <- plotGraph("New Deaths","deathIncrease","Dperday","New Deaths per Day",0)
Dperday_plot

ICU_plot <- plotGraph("ICU Cases","inIcuCumulative","Ihcrit","Cumulative ICU Cases",0)
ICU_plot
