library(magick)
library(ggplot2)
library(maps)
library(dplyr)
library(Hmisc)
library(stringr)
library(ggmap)
library(ggthemes)
library(png)
library(data.table)
library(leaflet)
library(sf)
# get the data for Georgia Map
overviewData = read.csv('./dataInput/countycases.csv', as.is = TRUE)
GAmap <<- st_read('https://opendata.arcgis.com/datasets/dc20713282734a73abe990995de40497_68.geojson')
## Table top 10:
GA_population = read.csv('./dataInput/POPsize.csv', as.is = TRUE)
GA_population <- GA_population[,c('FIP','POPSIZE')]
GAmap$GEOID10 <- as.numeric(GAmap$GEOID10)
GAmap <- inner_join(GAmap, GA_population, by = c("GEOID10" = "FIP"))



MergedGA_today <- inner_join(GAmap, overviewData[,c("county_resident",'Positive')], by = c('NAME10' = 'county_resident'))
MergedGA_today$CDensity <- round((MergedGA_today$Positive/(MergedGA_today$POPSIZE/100000)),2)

# ##########################################################
# dataTracking = fread('https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv')
# dataTracking <- as.data.frame(dataTracking)
# #dataTracking$COUNTY_NAME <- tolower(dataTracking$COUNTY_NAME)
# #dataTracking[which(dataTracking == 'dekalb', arr.ind = TRUE)] <- 'de kalb'
# georgiaIndex <- which(dataTracking[,'PROVINCE_STATE_NAME'] == 'Georgia', arr.ind = TRUE)
# georgiadataTracking <- dataTracking[georgiaIndex,]
# 
# georgiaData <- georgiadataTracking[,c('COUNTY_NAME','REPORT_DATE','PEOPLE_POSITIVE_CASES_COUNT')]
# georgiaData <- georgiaData%>% 
#   rename(
#     Positive = names(georgiaData)[3],
#     date = REPORT_DATE,
#     County = COUNTY_NAME
#   )
# georgiaData$date <- as.Date(georgiaData$date)

# GEORGIA CONFIRMED CASES MAP TODAY:
MergedGA_today$log <- log(MergedGA_today$CDensity)
MergedGA_today[which(MergedGA_today$log == "-Inf"),'log'] <- 'NA'
bins <- log(quantile(seq(0,max(MergedGA_today$CDensity), by = 1),c(0,0.01,0.1,.25,0.35,.5,0.75,0.95,1)))
bins[1] <- 0
bins_label <- c()
for(idx in 1:length(bins)-1){
  bins_label <- c(bins_label, paste0(round(exp(bins[idx]),0), 
                                     " – ", 
                                     round(exp(bins[idx + 1]),0)))
}
bins_label[1] <- '< 1'
#'khaki2','lightpink','orange',
positive_map_today <- ggplot() + geom_sf( data=MergedGA_today, 
                                               aes(fill = log), 
                                               color="black", size = 0.1)
positive_map_today <- positive_map_today + scale_fill_gradientn(colours = c('ghostwhite',"lemonchiffon",'lightyellow','bisque1','peachpuff2','coral',"red",'darkred'),
                                                                name="Cumulative Cases per 100K",
                                                                na.value = "gray",
                                                                limits = c(0,max(MergedGA_today$log)),
                                                                breaks = bins, labels = bins_label,
                                                                guide = guide_legend(
                                                                  keyheight = unit(2.5, units = "mm"),
                                                                  title.position = 'top',
                                                                  reverse = T)) +
  coord_sf() + theme_map() + 
  labs(title=paste('Georgia Confirmed Cases per 100K (', Sys.Date(), ')', sep='')) +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = c(0.85,0.55),
        legend.text = element_text(size=5),
        legend.title = element_text(size = 7)) 
ggsave(paste('./image/PositiveMap/PC_GA_Map_', Sys.Date(), '.png', sep=''),
       width = 6, height = 3.1, units = 'in', dpi=150, pointsize=8)
# statistics <- function(rdate){
#   subData <- georgiaData[which(georgiaData[,"date"] == rdate),]
#   return(subData)
# }
# ############## CREATE A SERIES OF IMAGES FROM THE PAST UNTIL NOW -1
# create_png <- function(rdate){
#   # draw base map
#   subdata <- statistics(rdate)
#   # combined two tables: This may no include the Non-Georgia Resident and Unknown.
#   MergedGA <- inner_join(GAmap, subdata, by = c('NAME10' = 'County'))
#   MergedGA$CDensity <- round((MergedGA$Positive/(MergedGA$POPSIZE/100000)),2)
#   MergedGA$log <- log(MergedGA$CDensity)
#   MergedGA[which(MergedGA$log == "-Inf"),'log'] <- NA
#   positive_map <- ggplot() + geom_sf( data=MergedGA,
#                                           aes(fill = log),
#                                           color="black", size = 0.1)
#   positive_map <- positive_map + scale_fill_gradientn(colours = c('ghostwhite',"lemonchiffon",'lightyellow','bisque1','peachpuff2','coral',"red",'darkred'),
#                                                                   name="Cumulative Cases per 100K",
#                                                                   na.value = "gray",
#                                                                   limits = c(0,max(MergedGA_today$log)),
#                                                                   breaks = bins, labels = bins_label,
#                                                                   guide = guide_legend(
#                                                                     keyheight = unit(2.5, units = "mm"),
#                                                                     title.position = 'top',
#                                                                     reverse = T)) +
#     coord_sf() + theme_map() +
#     labs(title=paste('Georgia Confirmed Cases per 100K (', rdate, ')', sep='')) +
#     theme(plot.title = element_text(face = "bold")) +
#     theme(legend.position = c(0.85,0.55),
#           legend.text = element_text(size=5),
#           legend.title = element_text(size = 7)) 
#   ggsave(paste('./image/PositiveMap/PC_GA_Map_', rdate, '.png', sep=''),
#          width = 6, height = 3.1, units = 'in', dpi=150, pointsize=8)
#   return(positive_map)
# }
# 
# # Read the individual maps into a data structure for use with 'magick'
# # draw base map
# 
# date_range <- seq(as.Date("2020-03-01"), Sys.Date()-1, "days")
# for (val in 1:length(date_range)){
#   create_png(date_range[val])
# }

### REMEMBER CHANGE SYS.DATE() - 1 to SYS.DATE()

date_range2 <- seq(as.Date("2020-03-01"), Sys.Date(), "days")

imglayers <- sapply(date_range2, function(x) {
  image_read(paste('./image/PositiveMap/PC_GA_Map_', x,'.png', sep=''))
})

# Generate an animated GIF with the individual maps and write to a file
imganim <- image_animate(image_join(imglayers), fps = 10, dispose = "previous")
mapanim <- image_write(imganim, './www/GAConfirmedCasesMap.gif')  
