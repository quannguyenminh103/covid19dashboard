library(tidyverse)
library(dplyr)
#library(plyr)

overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)
head(overviewData)
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

topTable <- topTable%>%
  rename(
    County = county_resident,
    Death = DEATHS, 
    Hospitalization = HOSPITALIZATION,
    Rate = case_rate
  )

topTable
