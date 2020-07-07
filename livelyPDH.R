# library(ggplot2)
# library(maps)
# library(dplyr)
# library(Hmisc)
# library(stringr)
# library(Hmisc)
# #library(ggmap)
# library(ggthemes)
# library(plotly)
# get the data for Georgia Map
GAmap <- map_data('county', 'georgia')

# read the CSV file
dataFile = read.csv('./dataInput/countycases.csv', as.is = TRUE)

# turn all counties into lowercase
dataFile$county_resident = tolower(dataFile$county_resident)
# Because there is an exception (difference) between our data file and available GAmap data from RStudio 
# dekalb vs de kalb
dataFile[which(dataFile == 'dekalb', arr.ind = TRUE)] <- 'de kalb'

# combined two tables: This may no include the Non-Georgia Resident and Unknown.
MergedGA <- inner_join(GAmap, dataFile, by = c("subregion" = "county_resident"))
# get the capitalized county names:
MergedGA$subregion = capitalize(MergedGA$subregion)
MergedGA <- MergedGA%>%
  rename(
    County = subregion,
    Death = DEATHS, 
    Hospitalization = HOSPITALIZATION
  )
# GEORGIA CONFIRMED CASES MAP:

staticPlot <- function(status,lowColor,highColor){
  status_map <- ggplot() + geom_polygon( data=MergedGA, 
                                           aes(x = long, y = lat, group = group, text = paste0('<b>County: </b>',County, "<br>", "<b>",status," Cases: </b>", MergedGA[,status]), fill = MergedGA[,status]), 
                                           color="black", size = 0.2) 
  status_map <- status_map + scale_fill_continuous(low = lowColor, high = highColor, limits = c(0,max(MergedGA[,names(MergedGA) == status])), 
                                                   breaks= quantile(MergedGA[,names(MergedGA) == status],c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1)),
                                                   na.value = "grey50") +
    coord_map("polyconic") + theme_map() +
    labs(title=paste0("Georgia ",status," Cases Map")) + theme(legend.position = "none") + 
    theme(plot.title = element_text(face = "bold")) + guides(fill = FALSE) +
    theme(plot.title = element_text(hjust=0.4)) #+ theme(plot.title = element_text(vjust= -10)) 
  status_map <- ggplotly(status_map, tooltip = 'text') %>%
    highlight(
      "plotly_hover",
      selected = attrs_selected(line = list(color = "black"))
    ) 
  return(status_map)
}

positive_map <- staticPlot('Positive','lemonchiffon','firebrick')
death_map <- staticPlot('Death','lightblue','darkblue')
hospitalization_map <- staticPlot('Hospitalization',"darkolivegreen1",'green4')
