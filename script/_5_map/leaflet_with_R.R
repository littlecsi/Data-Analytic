# install.packages("leaflet")
library(leaflet)
library(maps)
library(rgdal)

### Json
seattle_beat <- fromJSON("script/_1_database/seattle_beat.json")
seattle_beat <- as.data.frame(seattle_beat)

### Leaflet
shp_path <- "script/_5_map/leaflet-master/docs/shp/cb_2013_us_state_20m.shp"
states <- readOGR(shp_path,
                  layer = "cb_2013_us_state_20m",
                  GDAL1_integer64_policy = TRUE)

washington <- subset(states, states$STUSPS %in% c("WA"))

seattle <- leaflet(washington) %>% 
  addProviderTiles(providers$CartoDB.Positron)
  
seattle

topoData <- readLines("dataset/US_Counties.json") %>% paste(collapse = "\n")
leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
  addTiles() %>%
  addTopoJSON(topoData, weight = 1, color = "#444444", fill = FALSE)
