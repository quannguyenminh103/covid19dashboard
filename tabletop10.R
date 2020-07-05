library(tidyverse)
library(dplyr)
#library(plyr)

overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)

index <- order(-overviewData$Positive)
topTable <- overviewData[index,]
head(topTable)
topTable <- topTable%>%
  rename(
    County = county_resident,
    Death = DEATHS, 
    Hospitalization = HOSPITALIZATION,
    Rate = case_rate
  )


