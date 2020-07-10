# library(dplyr)
# library(Hmisc)
# library(stringr)
# library(Hmisc)
# library(ggthemes)
# library(plotly)
# library(leaflet)
# library(sf)

# get the data for Georgia Map
GAcounty <<- st_read('https://opendata.arcgis.com/datasets/dc20713282734a73abe990995de40497_68.geojson')

# read the CSV file
dataFile = read.csv('./dataInput/countycases.csv', as.is = TRUE)
dataFile <- dataFile[order(dataFile$county_resident),]

GA_sf <- inner_join(GAcounty,dataFile, by = c("NAME10"  = "county_resident"))
GA_sf <- GA_sf%>%
  rename(
    County = NAME10,
    Death = DEATHS, 
    Hospitalization = HOSPITALIZATION
  )

GA_sf <- GA_sf[order(GA_sf$County),]
getColors <- function(status,colorBands){
  bins <- quantile(dataFile[,status],c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1))
  pal <- colorBin(colorBands, domain = dataFile[,status], bins = bins)
  return(pal)
}  
#### Map
livelyMap <- function(status,name,colorBands){
  bins <- quantile(dataFile[,status],c(0,0.2,0.3,0.5,0.6,0.7,0.8,0.85,0.88,0.9,0.93,0.98,0.99,1))
  pal <- colorBin(colorBands, domain = dataFile[,status], bins = bins)
  
  map <- leaflet(GA_sf) %>%
    addPolygons(
      data = GA_sf,
      fillColor = ~pal(dataFile[,status]),
      weight = 0.5,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = sprintf("<strong>County: %s</strong><br>%s: %g", dataFile$county_resident, name, dataFile[,status]) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) #%>%
    #addLegend(pal = pal, values = ~dataFile[,status], opacity = 0.7, title = name,
     #         position = "bottomright")
  map <- map %>%   fitBounds(-83, 35, -83, 30.5)
  return(map)
}
positive_map <- livelyMap('Positive','Confirmed Cases',"YlOrRd")
death_map <- livelyMap('DEATHS',"Death Cases", "YlGnBu")
#hospitalization_map <- livelyMap('HOSPITALIZATION',"Hospitalizations", "PuRd")

