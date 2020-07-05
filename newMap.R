library(magick)
library(ggplot2)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
library(Hmisc)
#library(ggmap)
library(ggthemes)
library(png)

# get the data for Georgia Map
GAmap <- map_data('county', 'georgia')

# read the CSV file
overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)

# turn all counties into lowercase
overviewData$county_resident = tolower(overviewData$county_resident)
# Because there is an exception (difference) between our data file and available GAmap data from RStudio 
# dekalb vs de kalb
overviewData[which(overviewData == 'dekalb', arr.ind = TRUE)] <- 'de kalb'

# combined two tables: This may no include the Non-Georgia Resident and Unknown.
MergedGA_today <- inner_join(GAmap, overviewData, by = c("subregion" = "county_resident"))
# get the capitalized county names:
MergedGA_today$subregion = capitalize(MergedGA_today$subregion)

##########################################################
dataTracking = read.csv('./dataInput/COVID-19-Activity.csv', as.is = TRUE)
dataTracking$COUNTY_NAME <- tolower(dataTracking$COUNTY_NAME)
dataTracking[which(dataTracking == 'dekalb', arr.ind = TRUE)] <- 'de kalb'
head(dataTracking)

georgiaIndex <- which(dataTracking[,4] == 'Georgia', arr.ind = TRUE)
georgiadataTracking <- dataTracking[georgiaIndex,]
georgiaData <- georgiadataTracking[,c(2,3,1)]
# turn all counties into lowercase
georgiaData$COUNTY_NAME <- tolower(georgiaData$COUNTY_NAME)
georgiaData <- georgiaData%>% 
  rename(
    Positive = names(georgiaData)[3],
    date = REPORT_DATE
  )
georgiaData$date<- strptime(as.character(georgiaData$date), "%m/%d/%Y")
georgiaData$date <- format(georgiaData$date, "%Y-%m-%d")
georgiaData$date <- as.Date(georgiaData$date)

statistics <- function(rdate){
  subData <- georgiaData[which(georgiaData[,"date"] == rdate),]
  return(subData)
}
###############

create_png <- function(rdate){
  # draw base map
  subdata <- statistics(rdate)
  # combined two tables: This may no include the Non-Georgia Resident and Unknown.
  MergedGA <- inner_join(GAmap, subdata, by = c("subregion" = "COUNTY_NAME"))
  county <- map_data("county")
  gacounty <- county[which(county[,5] == 'georgia'),]
  gg <- ggplot()
  gg <- gg + geom_map(data=gacounty, map=gacounty,
                      aes(long, lat, map_id=region),
                      color="#2b2b2b", fill=NA, size=0.15)
  gg <- gg + coord_map("polyconic")
  gg <- gg + theme_map()
  gg <- gg + theme(plot.margin=margin(20,20,20,20))
  
  positive_map <- gg
  positive_map <- positive_map + geom_polygon( data=MergedGA, 
                                               aes(long, lat, group=group, fill = Positive), 
                                               color="white", size = 0.2)
  positive_map <- positive_map + scale_fill_continuous(name="Level of Positive Cases", 
                                                       low = "lemonchiffon", high = "firebrick",limits = c(0,max(MergedGA_today$Positive)), 
                                                       breaks=c(2,10000,2000,3000,4000,5000,6000), na.value = "grey50") +
    labs(title=paste('Georgia Confirmed Cases (', rdate, ')', sep='')) + theme(legend.position = "right")
  ggsave(paste('./www/Map_', rdate, '.png', sep=''),
         width = 6, height = 3.1, units = 'in', dpi=150, pointsize=14)
  return(positive_map)
}
# Read the individual maps into a data structure for use with 'magick'

#date_range <- seq(as.Date("2020-03-01"), Sys.Date()-1, "days")
# for (val in 1:length(date_range)){
#   create_png(toString(date_range[val]))
# }

# GEORGIA CONFIRMED CASES MAP TODAY:
county <- map_data("county")
gacounty <- county[which(county[,5] == 'georgia'),]
gg_today <- ggplot()
gg_today <- gg_today + geom_map(data=gacounty, map=gacounty,
                                aes(long, lat, map_id=region),
                                color="#2b2b2b", fill=NA, size=0.15)
gg_today <- gg_today + coord_map("polyconic")
gg_today <- gg_today + theme_map()
gg_today <- gg_today + theme(plot.margin=margin(20,20,20,20))
positive_map_today <- gg_today
positive_map_today <- positive_map_today + geom_polygon( data=MergedGA_today, 
                                                         aes(x=long, y=lat, group=group, fill = Positive), 
                                                         color="white", size = 0.2)
positive_map_today <- positive_map_today + scale_fill_continuous(name= "Level of Positive Cases", 
                                                                 low = "lemonchiffon", high = "firebrick",limits = c(0,max(MergedGA_today$Positive)), 
                                                                 breaks=c(2,10000,2000,3000,4000,5000,6000), na.value = "grey50") +
  labs(title=paste('Georgia Confirmed Cases (', Sys.Date()-1, ')', sep='')) + theme(legend.position = "right")
ggsave(paste('./www/Map_', Sys.Date(), '.png', sep=''),
       width = 6, height = 3.1, units = 'in', dpi=150, pointsize=14)

### REMEMBER CHANGE SYS.DATE() - 1 to SYS.DATE()

imglayers <- function(rdate){
  image_read(paste('./www/Map_', x,'.png', sep=''))
}

date_range2 <- seq(as.Date("2020-03-01"), Sys.Date(), "days")

imglayers <- sapply(date_range2, function(x) {
  image_read(paste('./www/Map_', x,'.png', sep=''))
})
  
  # Generate an animated GIF with the individual maps and write to a file
imganim <- image_animate(image_join(imglayers), fps = 10, dispose = "previous")
mapanim <- image_write(imganim, './GAConfirmedCasesMap.gif')  
  
  
  