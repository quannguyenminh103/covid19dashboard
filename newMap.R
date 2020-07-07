library(magick)
library(ggplot2)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
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
dataTracking = read.csv(url('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv'))
dataTracking$COUNTY_NAME <- tolower(dataTracking$COUNTY_NAME)
dataTracking[which(dataTracking == 'dekalb', arr.ind = TRUE)] <- 'de kalb'
#head(dataTracking)

georgiaIndex <- which(dataTracking[,4] == 'Georgia', arr.ind = TRUE)
georgiadataTracking <- dataTracking[georgiaIndex,]
georgiaData <- georgiadataTracking[,c(2,3,1)]
# turn all counties into lowercase
georgiaData$COUNTY_NAME <- tolower(georgiaData$COUNTY_NAME)
georgiaData <- georgiaData%>% 
  rename(
    Positive = names(georgiaData)[3],
    date = REPORT_DATE,
    County = COUNTY_NAME
  )
georgiaData$date<- strptime(as.character(georgiaData$date), "%m/%d/%Y")
georgiaData$date <- format(georgiaData$date, "%Y-%m-%d")
georgiaData$date <- as.Date(georgiaData$date)

# statistics <- function(rdate){
#   subData <- georgiaData[which(georgiaData[,"date"] == rdate),]
#   return(subData)
# }
# ###############
# 
# create_png <- function(rdate){
#   # draw base map
#   subdata <- statistics(rdate)
#   # combined two tables: This may no include the Non-Georgia Resident and Unknown.
#   MergedGA <- inner_join(GAmap, subdata, by = c("subregion" = "COUNTY_NAME"))
#   positive_map <- ggplot() + geom_polygon( data=MergedGA, 
#                                                aes(long, lat, group=group, fill = Positive), 
#                                                color="black", size = 0.2)
#   positive_map <- positive_map + scale_fill_continuous(name="Level of Positive Cases", 
#                                                        low = "lemonchiffon", high = "firebrick",limits = c(0,max(MergedGA_today$Positive)), 
#                                                        breaks=quantile(MergedGA_today$Positive,c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1)), na.value = "grey50") +
#     coord_map() + theme_map() +  
#     labs(title=paste('Georgia Confirmed Cases (', rdate, ')', sep='')) +
#     theme(plot.title = element_text(face = "bold")) +
#     theme(legend.position = 'none') 
#   ggsave(paste('./www/PositiveMap/PC_GA_Map_', rdate, '.png', sep=''),
#          width = 6, height = 3.1, units = 'in', dpi=150, pointsize=14)
#   return(positive_map)
# }
# Read the individual maps into a data structure for use with 'magick'

date_range <- seq(as.Date("2020-03-01"), Sys.Date()-1, "days")
for (val in 1:length(date_range)){
  create_png(toString(date_range[val]))
}
# GEORGIA CONFIRMED CASES MAP TODAY: c(2,100,300,500,700,800,1000,3000,5000,6000,7000,9000)
positive_map_today <- ggplot() + geom_polygon( data=MergedGA_today, 
                                                         aes(x=long, y=lat, group=group, fill = Positive), 
                                                         color="black", size = 0.2)
positive_map_today <- positive_map_today + scale_fill_continuous(name= "Level of Positive Cases", 
                                                                 low = "lemonchiffon", high = "firebrick",limits = c(0,max(MergedGA_today$Positive)), 
                                                                 breaks= quantile(MergedGA_today$Positive,c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1)), na.value = "grey50") +
  coord_map() + theme_map() +  
  labs(title=paste('Georgia Confirmed Cases (', Sys.Date(), ')', sep='')) +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = 'none')
ggsave(paste('./www/PositiveMap/PC_GA_Map_', Sys.Date(), '.png', sep=''),
       width = 6, height = 3.1, units = 'in', dpi=150, pointsize=14)
### REMEMBER CHANGE SYS.DATE() - 1 to SYS.DATE()

# imglayers <- function(rdate){
#   image_read(paste('./www/PositiveMap/PC_GA_Map_', x,'.png', sep=''))
# }

date_range2 <- seq(as.Date("2020-03-01"), Sys.Date(), "days")

imglayers <- sapply(date_range2, function(x) {
  image_read(paste('./www/PositiveMap/PC_GA_Map_', x,'.png', sep=''))
})
  
  # Generate an animated GIF with the individual maps and write to a file
imganim <- image_animate(image_join(imglayers), fps = 10, dispose = "previous")
mapanim <- image_write(imganim, './www/GAConfirmedCasesMap.gif')  

