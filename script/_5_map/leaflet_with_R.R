# install.packages("leaflet")
library(leaflet)
library(maps)
library(rgdal)
library(albersusa)

### Json


### Leaflet
shp_path <- "script/_5_map/leaflet-master/docs/shp/cb_2013_us_state_20m.shp"
states <- readOGR(shp_path,
                  layer = "cb_2013_us_state_20m",
                  GDAL1_integer64_policy = TRUE)

washington <- subset(states, states$STUSPS %in% c("WA"))

seattle <- leaflet(washington) %>% 
  addProviderTiles(providers$CartoDB.Positron)
  
seattle