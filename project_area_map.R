library(tigris)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(readxl)
library(htmlwidgets)
library(tidyverse)
library(tigris)
library(tidycensus)
library(scales)
library(shiny)
library(xlsx)
library(htmltools)
library(googlesheets4)
library(geojsonsf)
library(rgdal)
library(leaflet.extras)
library(mapview)
# library(HydroData)
sf_use_s2(FALSE)
setwd("~/Documents/GitHub/A2-mapping")
filenames = c("ACV project area",
              "JC project area",
              "LHI project area",
              "LG Inc project area",
              "NEOBHC project area",
              "SBU project area",
              "WPA project area"
              )
huc8 = st_read("~/Documents/GitHub/A2-mapping/shapefiles/HUC8_CONUS/HUC8_US.shp")
for (i in seq_along(filenames)){
# i = 1
  path = paste0("~/Documents/GitHub/A2-mapping/shapefiles/",filenames[i],"/area-of-interest.shp")
sf = st_read(path)

st_crs(sf) = st_crs(huc8)
# test = st_intersection(huc8,sf)
test = st_intersection(huc8,sf)
hucAOI = huc8 %>% filter(TNMID %in% test$TNMID)
# plot(test)
# sf = st_transform(sf, 4139)
# pr = counties(state = "PR")
# st_crs(sf) = st_crs(pr)

map1 = leaflet(options = leafletOptions(zoomControl = FALSE,
                                 minZoom = 8, maxZoom = 8))  %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  # addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = hucAOI, group = "watershed", weight = 1, color = "#48b8d9", opacity = 1, fillOpacity = 0.5,
              # fillColor = ~binpal()(lead_map()$pct_pipe_replaced),
              # fillColor = ~colormap(pct_pipe_replaced),
              highlightOptions = highlightOptions(color = "Grey", weight = 3,
                                                  bringToFront = TRUE),
              # label = labels(),
              # labelOptions = labelOptions(noHide = F, textsize = "15px"))
  ) %>%
  addPolygons(data = sf, group = "AOI", weight = 1, color = "red", opacity = 1, fillOpacity = 0.8,
              # fillColor = ~binpal()(lead_map()$pct_pipe_replaced),
              # fillColor = ~colormap(pct_pipe_replaced),
              highlightOptions = highlightOptions(color = "Grey", weight = 3,
                                                  bringToFront = TRUE),
              # label = labels(),
              # labelOptions = labelOptions(noHide = F, textsize = "15px"))
  ) %>% addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = TRUE)
  ) %>% 
  setMapWidgetStyle( style = list(background = "transparent")) 
map1
mapshot(map1,file = paste0(filenames[i]," street.pdf"), remove_controls = FALSE)
}
# write_sf(sf, "LG_new.shp" )
