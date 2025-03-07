library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)
library(googlesheets4)

# Deauthorize Google Sheets

gs4_deauth()

# Load location data
URL <- "https://docs.google.com/spreadsheets/d/1aBRXRiL65Q9QKj3AmA005sKWh6MNgCkMYgdsuQu_n7s/edit#gid=0"
location <- read_sheet(URL) %>%
  mutate(across(everything(), as.character)) %>%  # Ensure all columns are characters
  mutate(lat = as.numeric(unlist(lat)), 
         lon = as.numeric(unlist(lon))) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("body { font-family: 'Open Sans', sans-serif; }"))),
  
  absolutePanel(top = 1, right = 50,
                h5("Action Buttons:"),
                actionButton("Bbutton", "Zoom Out"),
                style = "opacity: 1; z-index: 10;
                          background: #F5F5F5;
                          border-radius: 10px;
                          padding: 10px;"
  ),
  
  absolutePanel(
    top = 110, right = 50,
    selectInput(inputId = "Issue",
                selected = "All",
                label = "Select an issue: ",
                choices = c("All", "Air Pollution", "Drought", "Flooding", "Groundwater Contamination", "Hazardous/Toxic Sites", "Heat", "Hurricanes/Tropical Storms", "PFAS/PFOS", "Sewage/Sewage Treatment", "Water Contamination", "Wildfires")),
    style = "opacity: 1; z-index: 10;
              background: #F5F5F5;
              border-radius: 10px;
              padding: 10px;"
  ),
  
  leafletOutput("Map", height = "100vh")
)

server <- function(input, output, session) {
  
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = location,
                 layerId = ~`Name of group`,
                 label = ~`Name of group`,
                 clusterOptions = markerClusterOptions()) %>%
      setView(lat = 41.62531, lng = -90.71755, zoom = 4)
  })
  
  observeEvent(input$Issue, {
    location_filtered <- if(input$Issue == "All") {
      location
    } else {
      location %>% filter(grepl(input$Issue, `Which of the following climate or environmental impacts affects your community?`))
    }
    
    leafletProxy("Map") %>%
      clearMarkers() %>% clearMarkerClusters() %>%
      addMarkers(data = location_filtered,
                 label = ~`Name of group`,
                 layerId = ~`Name of group`,
                 clusterOptions = markerClusterOptions())
  })
  
  observeEvent(input$Bbutton, {
    leafletProxy("Map") %>% setView(lat = 41.62531, lng = -90.71755, zoom = 4)
  })
  
  observeEvent(input$Map_marker_click, {
    click <- input$Map_marker_click
    if (!is.null(click$id)) {
      url <- location %>% filter(`Name of group` == click$id) %>% pull(`Member page url`)
      if (!is.na(url) && url != "") {
        shinyjs::runjs(sprintf("window.open('%s', '_blank')", url))  # Open URL in a new tab
      }
    }
  })
}

shinyApp(ui = ui, server = server)