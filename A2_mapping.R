library(tigris)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(readxl)
library(htmlwidgets)
library(tidyverse)
library(tidycensus)
library(gridExtra)
library(ggmap)

setwd("~/Documents/GitHub/A2-mapping")
membership_df = read_xlsx("Master Metrics Sheet - EPIC.xlsx")
membership_df$Address = paste(membership_df$Town,membership_df$State)
register_google("AIzaSyCp56yhymS4rcUfPw_X7egiLLuSxhfh4PY")
locs <- c('Birmingham AL', 'Seattle WA',"Alaska Community Action on Toxics")
x = geocode(membership_df$`Name of group`)
membership_df = cbind(membership_df,x)
na_coord = membership_df %>% filter(is.na(membership_df$lon) == TRUE)
na_coord_x = geocode(na_coord$Address)
na_coord = cbind(na_coord,na_coord_x)
na_coord = na_coord[,-(12:13)]
na_coord = na_coord %>% rename(lat_1 = lat, lon_1 =lon ) %>% select(`Name of group`,"lon_1","lat_1")

membership_df = membership_df %>% left_join(na_coord, by = 'Name of group')
membership_df = membership_df %>% mutate(lat = ifelse(is.na(lat),lat_1, lat),
                                         lon = ifelse(is.na(lon),lon_1, lat))
location = membership_df
census_api_key("165e841c61a90d08a11cdb6b6825fa95286df6d5",install = TRUE)

y = geocode(location$Address)
colnames(y) = c("lon","lat")
location = cbind(location,y)


### add video to leaflet map popup
videolink = '<iframe width="300" height="169" src="https://www.youtube.com/embed/YftutAJeXAU?showinfo=0" frameborder="0" allowfullscreen></iframe>'

leaflet(location) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label =~`Name of group`,
            
             clusterOptions = markerClusterOptions(),
             popup = ~paste("<b>Name:</b>",`Name of group`,
                            "<br>",
                            '<iframe width="300" height="169" src="https://www.youtube.com/embed/YftutAJeXAU?showinfo=0" frameborder="0" allowfullscreen></iframe>',
                            "<br>", 
                            "<img src='https://thrivingearthexchange.org/wp-content/uploads/2021/08/Anthropocene-Alliance-Logo.jpg' width = 100 />")
             ) %>% 
  setView(lat = 41.62531, lng = -97.71755, zoom = 3)

#   addCircleMarkers(lng = ~lon, lat = ~lat, 
#                    popup = ~`Name of group`)


### hold off ACS data until next version. Focus on Highlevel info for About Us map first
## example code used for New Orleans project
# 
# data = get_acs(geography = "block group",
#         state = "LA",
#         year = 2020,
#         survey = "acs5",
#         variable = c("B03002_001E","B03002_003E"),
#         output = "wide",
#         geometry = TRUE) %>% filter(grepl("Orleans Parish",NAME))
# data2 = get_acs(geography = "block group",
#                 state = "LA",
#                 year = 2020,
#                 survey = "acs5",
#                 table = "B19013", # median household income
#                 output = "wide",
#                 geometry = FALSE) %>% filter(grepl("Orleans Parish",NAME))
# data3 = get_acs(geography = "block group",
#                 state = "LA",
#                 year = 2020,
#                 survey = "acs5",
#                 table = "C17002", # percent below poverty line
#                 output = "wide",
#                 geometry = FALSE) %>% filter(grepl("Orleans Parish",NAME))
# data = data %>% left_join(data2 %>% select(-c("B19013_001M","NAME")),by= "GEOID")
# data3 = data3 %>% select(GEOID,
#                          C17002_001E, # total population evaluated for poverty
#                          C17002_002E, #<50% FPL
#                          C17002_003E # 50-100% FPL
#                          ) %>% mutate(pct_below_FPL =( C17002_002E + C17002_003E)/C17002_001E)
# 
# 
# # data = st_as_sf(data)
# data = data %>% mutate(percent = (B03002_001E-B03002_003E)/B03002_001E)
# l1 = leaflet(location) %>% addTiles() %>%
#   addCircleMarkers(lng = ~Lon, lat = ~Lat, 
#                    popup = ~Address)
# pal= colorNumeric(palette = "Reds",
#                   domain = data$percent)
# l2 = leaflet() %>% addPolygons(data  = data, color = "black", weight = 0.5, smoothFactor = 0.5,
#                         opacity = 1.0, fillOpacity = 0,
#                         fillColor = ~pal(percent),
#                         highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                             bringToFront = TRUE),
#                         label = data$NAME) %>% addTiles() %>% 
#   addCircleMarkers(data = location, lng = ~Lon, lat = ~Lat,  popup = ~Address)
#   
# l2
# 
# 
# location_sf = location %>%
#   mutate_at(vars(Lon, Lat), as.numeric) %>%   # coordinates must be numeric
#   st_as_sf(
#     coords = c("Lon", "Lat"),
#     agr = "constant",
#     crs = st_crs(data),        # same as the acs data
#     stringsAsFactors = FALSE,
#     remove = TRUE
#   )
# 
# gardens_in_BG = st_join(location_sf, data, join = st_within) 
# 
# gardens_BG_count = count(as_tibble(gardens_in_BG), NAME)  
# gardens_in_BG = gardens_in_BG %>% select(GEOID, NAME, Address, B03002_001E, B03002_003E, percent, B19013_001E)
# gardens_in_BG = gardens_in_BG %>% left_join(location %>% select(Address, Lon, Lat, "Area of Rain Garden"), by = "Address")
# gardens_in_BG = gardens_in_BG %>% left_join(data3 %>% select(GEOID, pct_below_FPL), by = "GEOID")
# rainfall =  60.12
# # gardens_in_BG = gardens_in_BG %>% rename("area_sqft"= "Area of Rain Garden" )
# # gardens_in_BG = gardens_in_BG %>% rename("percent_non_white"= "percent" )
# gardens_in_BG = gardens_in_BG %>% mutate(annual_water_captured_gal = 60.12 * 0.6 * area_sqft)
# # pal= colorNumeric(palette = "Reds",
# #                   domain = gardens_in_BG$percent_non_white)
# data_lite = data%>% filter(GEOID %in% gardens_in_BG$GEOID)
# pal= colorNumeric(palette = "Blues",
#                   domain = data$B19013_001E)
# awesome <- makeAwesomeIcon(
#   icon = "info",
#   iconColor = "black",
#   markerColor = "blue",
#   library = "fa"
# )
# 
# # data = data %>% filter(NAME =)
# l3 = leaflet() %>% addPolygons(data  = data, color = "black", weight = 0.5, smoothFactor = 0.5,
#                                opacity = 1.0, fillOpacity = 0.3,
#                                fillColor = ~pal(B19013_001E),
#                                highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                                    bringToFront = TRUE),
#                                label = data$NAME) %>% addTiles()%>% addLegend("bottomright", pal = pal, values =data$B19013_001E,
#                                                                               title = "Median Household Income",
#                                                                               labFormat = labelFormat(prefix = "$"),
#                                                                               opacity = 1
#                                ) %>%
#   # # addCircleMarkers(data = location, lng = ~Lon, lat = ~Lat,  popup = ~Address)
#   addAwesomeMarkers(data = gardens_in_BG, ~Lon, ~Lat,
#                     popup = ~paste("<br>Rain Garden Address:",Address,
#                                    "<br>Block Group:", NAME,
#                                    "<br>Median Household Income in Dollars:", B19013_001E,
#                                    "<br>% non-white population:", round(percent_non_white * 100,1),
#                                    "<br>% Population Below Poverty:", round(pct_below_FPL * 100, 1),
#                                    "<br>Size of the Rain Garden in Square Feet:", area_sqft,
#                                    "<br> Gallons of Water Captured Annually:", round(annual_water_captured_gal,0)
#                     ),
#                     label = ~Address,
#                     icon = awesome)
# l3
#   # color = ~WaterFirst.Status
# gardens_in_BG_lite = gardens_in_BG %>% st_drop_geometry()
# plot_income_distribution = ggplot(gardens_in_BG_lite) + geom_histogram(aes(x = B19013_001E/1000), binwidth = 10) +labs(x = "Median Household Income, thousand Dollars")+ theme_bw()
# plot_income_distribution
# plot_poverty = ggplot(gardens_in_BG_lite) + geom_histogram(aes(x = pct_below_FPL), binwidth = 0.1) +labs(x = "Pencentage of Population Below Federal Poverty Line")+ theme_bw()
# plot_poverty
# plot_race = ggplot(gardens_in_BG_lite) + geom_histogram(aes(x = percent_non_white), binwidth = 0.1) +labs(x = "Percentage of Non-White Population") + theme_bw()
# plot_race
# plot_annual_water_captured = ggplot(gardens_in_BG_lite,aes(x = annual_water_captured_gal/1000)) +geom_histogram( binwidth = 50) +labs(x = "Stormwater Captured Annually, thousand Gallons")+ theme_bw()
# plot_annual_water_captured
# pdf("plots.pdf", width = 10, height = 8)
# grid.arrange(plot_income_distribution,
#              plot_poverty,
#              plot_race,
#              plot_annual_water_captured,nrow = 2)
# # plot_income_distribution
# # plot_poverty
# # plot_race
# # plot_annual_water_captured
# dev.off()

