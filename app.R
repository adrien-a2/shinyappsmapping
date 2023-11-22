# setwd("~/Documents/GitHub/A2-mapping")
# load("working_progress.RData")
library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(googlesheets4)
library(ggmap)
library(tigris)
library(shinyjs)
library(leaflet.extras)

gs4_deauth()
# URL <- "https://docs.google.com/spreadsheets/d/18QrhmTteBi6U4lSwAIBWnyqWKYoOPOiGCt7NHQRtf_Y/edit#gid=0"
# URL <- "https://docs.google.com/spreadsheets/d/1s53icFTg_ThYgyH22GoxrzFjo6RSJtrREMOCZ7pOFNM/edit#gid=0"
# URL2 <- "https://docs.google.com/spreadsheets/d/1UXWdn1Y-fnC_gTYdP4GxedAf9ohCwm2m1kVBVgE2M2Y/edit#gid=0"
URL <- "https://docs.google.com/spreadsheets/d/1aBRXRiL65Q9QKj3AmA005sKWh6MNgCkMYgdsuQu_n7s/edit#gid=0"
location = read_sheet(URL)
# aux_data = read_sheet(URL2)
# location = location %>% left_join(aux_data)
location = location   %>% mutate(lat = unlist(lat),
                                 lon = unlist(lon)) %>% as.data.frame() %>%
  filter(!lat == "NA") %>%
  # filter()
  st_as_sf(coords = c("lon","lat"), crs = 4326)




# states_sf = states()

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # includeCSS("www/CSS.css")
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
      body {
        font-family: 'Open Sans', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }
      .frame {
          overflow-y: auto;
          
          max-height: 200px;
          max-width: 100%;
          line-height: 1.5em;
      }
      
      .frame::-webkit-scrollbar {
          -webkit-appearance: none;
      }
      
      .frame::-webkit-scrollbar:vertical {
          width: 11px;
      }
      
      .frame::-webkit-scrollbar:horizontal {
          height: 11px;
      }
      
      .frame::-webkit-scrollbar-thumb {
          border-radius: 8px;
          border: 2px solid white; /* should match background, can't be transparent */
          background-color: rgba(0, 0, 0, .5);
      }             
"))
  ),
  

  
  # absolutePanel(
  #   # https://anthropocenealliance.org/wp-content/uploads/2022/10/Aa-logo-white-200x200-retina.png
  #   # HTML('<img border="0" alt="logo" src="https://photos.smugmug.com/photos/i-5v2NgGr/0/57503bb2/L/i-5v2NgGr-L.png" width="40" height="40">  <font size="+2">Anthropocence Alliance: Our Communities</font>'),
  #   HTML("<img border='0' alt='logo' src='https://photos.smugmug.com/photos/i-5v2NgGr/0/57503bb2/L/i-5v2NgGr-L.png' width='40' height='40'>
  #       <font size='+2'>Anthropocene Alliance: Click a </font> 
  #     <img border='0' src='https://photos.smugmug.com/photos/i-r9zgwVm/0/8e1b17be/L/i-r9zgwVm-L.png' alt='logo' width='25' height='40'> 
  #     <font size = '+2'> to learn more</font></span></p>" ),
  #   style = "font-weight: bold; 
  #             width: 106%;  
  #             text-align: center;
  #             left: -25px;
  #             opacity: 1; z-index: 10;
  #             background: #242526;
  #             color: white;
  #             position: relative;
  #             border-radius: 10px;
  #             padding: 10px;"),
  absolutePanel(top = 1, right = 50,
                h5("Action Buttons:"),
                actionButton("Sbutton","Show Sidebar"),
                actionButton("Bbutton","Zoom Out"),
                style = "opacity: 1; z-index: 10;
  background: #F5F5F5;
  border-radius: 10px;
  padding-right: 100px;
    padding-bottom: 20px;
    padding-left: 10px;
    padding-top: 10px;"
  ),
  
  absolutePanel(
                top = 110, right = 50,
                # selectInput(inputId = "Issue",
                #             selected = "All",
                #             label = "Select a climate/environmental issue:",
                #             choices = c("All","Flooding","Air Pollution","Water Contamination", "Drought","Heat")),
                selectInput(inputId = "Issue",
                            selected = "All",
                            label = "Select an issue: ",
                            choices = c("All",
                                        "Flooding",
                                        "Wildfires",
                                        "Heat",
                                        "Drougt",
                                        "Erosion/subsidence",
                                        "Hurricanes/tropical storms",
                                        "Earthquakes",
                                        "Logging/biomass",
                                        "Superfund sites",
                                        "Incinerator/dumping/landfill",
                                        "Lead contamination",
                                        "Hazardous/toxic sites",
                                        "Fracking/oil and gas development/pipelines",
                                       "Mining",
                                       "Nuclear power plants",
                                        "Coal/coke plants and emissions",
                                        "Industrial agriculture/animal waste",
                                        "PFAS/PFOS",
                                        "Noise/light pollution",
                                        "Port/transit/highway contamination/noise",
                                        "Sewage/sewage treatment",
                                        "Groundwater contamination",
                                        "Air pollution",
                                        "Hypoxia (oceanic dead zones)",
                                        "Fighting development/destruction of wildlife/extinction of species"
                                       
                                       
                                        )),
                
                
                # selectInput(inputId = "Strategy",
                #             label = "Select a strategy:",
                #             selected = "All",
                #             choices = c("All",
                #                         "Affordable Housing",
                #                         "Art Activism",
                #                         "Community Farm/Gardens",
                #                         "Community Land Trusts/Land Conservation",
                #                         "Community Science",
                #                         "Direct Relief and Aid",
                #                         "Elevation or Relocation of Homes",
                #                         "Fighting Industrial Contamination",
                #                         "Green Infrastructure",
                #                         "Halting Bad Development",
                #                         "Nature-Based Solutions",
                #                         "Policy Reform",
                #                         "Renewable Energy",
                #                         "Rights of Nature")),
                style = "opacity: 1; z-index: 10;
    background: #F5F5F5;
    border-radius: 10px;
    padding: 10px;"
                # setBackgroundColor("ghostwhite")
  ),
  
  
  sidebarLayout(
    div(id = "Sidebar",
    sidebarPanel(
      
      style = "position: fixed; 
      # height: 100vh; 
      width: 34.5%; 
      left: 2px;
      overflow-y: auto; 
      top: 1px;
      bottom: -17px;
      z-index: 10;", 
      div(style = "display:inline-block; float:left; margin-bottom: 20px"),
      
      uiOutput("SidebarTitle"), 
      div(id = "SidebarDescContent", 
          class = "frame",
          uiOutput("SidebarDescContent"),
          br(),
          uiOutput("SidebarHeaderContent"), 
          ),
      # uiOutput("SidebarDescContent", 
      #          style = 
      #            "max-height: 200px; 
      #          max-width: 100%; 
      #          overflow-y: scroll; 
      #          margin-right: -10px; 
      #          margin-bottom: 10px; 
      #          margin-top:10px;"),
     
      # uiOutput("video"),
      uiOutput("image"),
      HTML("<br><br><br>"),
      actionButton("Hbutton","Hide Sidebar"),
      width = 3
    )),
    
    leafletOutput("Map", height = "100vh"),
    # mainPanel(
    #   leafletOutput("Map", height = "100vh"),
    #   
    #   style = "right: 0px;
    #   width: 65%;
    #   bottom: 0px;
    #   position: fixed;"
    # ),
    
    
    position = c("left"), fluid = FALSE)
)


server <- function(input, output) {
  shinyjs::hide(id = "Sidebar")
  ### Create reactive dataframe for the interactive map ###
  AllDataReactive = reactiveValues(df = data.frame())
  AllDataReactive$df = location
  ### Main Map Body ###
  
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      # addProviderTiles(providers$GeoportailFrance.orthos, group = "Geoportail") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Toner Lite") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      # addLayersControl(baseGroups = c("Toner Lite","Geoportail","World Imagery"),position = "bottomright") %>%
      # addPolygons(data = states_sf, color = "black", weight = 1, label = ~STUSPS ) %>%
      addMarkers(data = location,
                 layerId =~`Name of group`,
                 label = ~`Name of group`,
                 clusterOptions = markerClusterOptions()
                 # clusterOptions = markerClusterOptions(
                 #   # iconCreateFunction= JS("
                 #   #  return new L.DivIcon({ html: '<b>' + cluster.getChildCount() + '</b>' }
                 #   # ")
                 #   iconCreateFunction= JS(
                 # 
                 #   "function (cluster) {
                 #    var childCount = cluster.getChildCount();
                 #    var c = ' marker-cluster-';
                 #    if (childCount < 5) {
                 #      c +=  'small';
                 #    } else if (childCount < 10) {
                 #       c +=  'medium';
                 #    } else {
                 #      c +=  'large';
                 #    }
                 #    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>',
                 #    className: 'marker-cluster' + c,
                 #    iconSize: new L.Point(40, 40)
                 #    });
                 # 
                 #  }"
                 #   )
                 # ),
                 
                 # popup = ~paste(paste0("<b>",`Name of group`,"</b>"),
                 #                "<br>",
                 #                "<b>Point of contact:</b>",`Leaders name`,
                 #                "<br>",
                 #                "<b>Issues:</b>",`Which of the following climate or environmental impacts affects your community?`,
                 #                "<br>",
                 #                "<b>Communities served:</b>",`Town`,
                 #                "<br>",
                 #                "<b>State:</b>",`State`,
                 #                "<br>"
                 # )
      ) %>%
      # addLegend(colors = c("green","orange","red"), labels = c("0-10","10-100","greater than 100"),title = 'Communities Count',
      #           position = 'bottomright') %>%
      # addControl(rr, position = "topleft")%>%
      setView(lat = 41.62531, lng = -90.71755, zoom = 4) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }")
    
  })
  
  observeEvent(input$Issue,{

    
    location_1 <- if(input$Issue == "All") {
      ## Option A all data
      AllDataReactive$df
    } else{
      
      # filter by Issue
      # AllDataReactive$df %>% filter(grepl(input$Issue, `Which of the following climate or environmental impacts affects your community?`))
      AllDataReactive$df %>% filter(grepl(input$Issue, `Which of the following climate or environmental impacts affects your community?`))
    }
    
    map = leafletProxy("Map") %>% 
      clearMarkers() %>% clearMarkerClusters()
    
    if (nrow(location_1) > 0){
      
      map %>% 
        addMarkers( data = location_1,
                    label = ~`Name of group`,
                    layerId = ~`Name of group`,
                    clusterOptions = markerClusterOptions()
                    # popup = ~paste(paste0("<b>",`Name of group`,"</b>"),
                    #                "<br>",
                    #                "<b>Point of contact:</b>",`Leaders name`,
                    #                "<br>",
                    #                "<b>Issues:</b>",`Which of the following climate or environmental impacts affects your community?`,
                    #                "<br>",
                    #                "<b>Communities served:</b>",`Town`,
                    #                "<br>",
                    #                "<b>State:</b>",`State`,
                    #                "<br>")
        )
    }
  })
  
  # set up zoom out button
  observeEvent(input$Bbutton,{
  map = leafletProxy("Map") %>% setView(lat = 41.62531, lng = -90.71755, zoom = 4)
  shinyjs::hide("Sidebar")
  })
  
  SelectedDataReactive = reactiveValues(df = data.frame())
  SelectedDataReactive$df = location %>% st_drop_geometry()
  ### reactive sidebar
  observeEvent(input$Map_marker_click,{
    click <- input$Map_marker_click
    if (is.null(click)) {
      SelectedDataReactive$df <- SelectedDataReactive$df[1,]
      # print(SelectedDataReactive$df$`Name of group`)
    } else{
      SelectedDataReactive$df <-  AllDataReactive$df %>%
        filter(`Name of group` == click$id) %>% st_drop_geometry()
    }
    print(SelectedDataReactive$df$`Name of group`)
    shinyjs::show(id = "Sidebar")
  })
  ## Sidebar Content
  # observeEvent(input$Map_marker_click, {
  #   click <- input$Map_marker_click
  #   SidebarDes <- if(is.null(click)) {
  #     "Select a Member"
  #   } else {
  #     SidebarReactive$df %>% filter(`Name of group` == click$id)%>% select(`Name of group`)
  #   }
  #   # print(SidebarDes)
  # })

  # Context switch observe 
  observeEvent(input$Sbutton, {
    print("Show Button working")
    shinyjs::show(id = "Sidebar")
    print("checkpoint 1")
    # toggle("Table", anim = FALSE,animType = "slide",time = 0.5)
  })
  observeEvent(input$Hbutton, {
    print("Hide Button working")
    shinyjs::hide(id = "Sidebar")
    print("checkpoint 2")
    # toggle("Table", anim = FALSE,animType = "slide",time = 0.5)
  })
  
  ## Sidebar title 
  output$SidebarTitle <- renderUI({
    req(SelectedDataReactive$df)### work on this feature
    if (is.null(input$Map_marker_click)){
      title = "Select a member"
    } else{
      title = as.character(SelectedDataReactive$df  %>% select(`Name of group`))
      # print(title)
      }
    h2(title)
  })
  
  # ## Sidebar Tabset Panel 
  # output$SidebarSelect <- renderUI({
  #   selectInput("ToolName", 
  #               "Select an EJ Tool:", 
  #               unique(SelectedDataReactive$df$Name))
  # })
  
  ## Sidebar Tabset Panel
  
  output$SidebarHeaderContent <- renderUI({
    
    req(SelectedDataReactive$df)### work on this feature
    if (is.null(input$Map_marker_click)){
      Link <- paste("<b>", "<a href='https://anthropocenealliance.org/our-communities/' target='_blank'>Go to Our Communities Page</a>", "</b>","<br>")
    
      } else{
        
      url = as.character(SelectedDataReactive$df  %>% select(`Member page url`))
      # print(url)
      Link <- paste("<b>", paste0("<a href='",url, " ' target='_blank'>See profile page for more information</a>"), "</b>","<br>")
      
      }
      # h2(title)
    
    
    # LastUpdated <- paste("<b>", "Last Updated:","</b>", ifelse(!is.na(SelectedTool$Updated_Date), SelectedTool$Updated_Date,SelectedTool$Published), "<br>")
    HTML(Link)
  })
  
  
  output$SidebarDescContent <- renderUI({
    
    req(SelectedDataReactive$df)### work on this feature
    if (is.null(input$Map_marker_click)){
      # Description <- paste("Select a member (click on a pin) on the map for details")
      Description <- paste("Welcome to the Anthropocene Alliance Member Map, where you'll discover the impactful work of our members across the United States. 
                            Each member represents a unique community facing pressing environmental and social issues. 
                            From rising sea levels to industrial pollution, our map showcases their initiatives and innovations.",
                           "<br/>",  
                           "<br/>",
                           "Explore inspiring stories of environmental restoration, policy advocacy, and community empowerment.
                            Witness the interconnectedness of these challenges, spanning sustainability, social justice, equity, and resilience.
                            Together, we can create a more sustainable and equitable future. 
                            Join us in celebrating the power of local action and building a stronger Anthropocene Alliance.")
    } else{
      
      Description = as.character(SelectedDataReactive$df  %>% select(`D1`))
      if (is.na(Description) == TRUE) {
        Description = ""
      }
    }
    
    
    
    # EJDef <- paste("<b>", "Environmental Justice Definition:","</b>", "<br>", ifelse(is.na(SelectedTool$EJ_Def_Det), "No Definition", SelectedTool$EJ_Def_Det), "<br>","<br>")
    HTML(paste0("<font size='+0.5'>",Description, "</font>")) 
  })
  output$video <- renderUI({
    Videolink <- paste(
      "<br>",
      '<iframe width="90%" height="300px" src="https://www.youtube.com/embed/YftutAJeXAU?showinfo=0" frameborder="0" allowfullscreen></iframe>',
      "<br>", 
      "<br>" )
    HTML(Videolink)
  })
  output$image <- renderUI({
   
    req(SelectedDataReactive$df)### work on this feature
    if (is.null(input$Map_marker_click)){
      # 
    } else{
      img_url = as.character(SelectedDataReactive$df  %>% select(`Photos links`))
      if (is.na(img_url) == TRUE) {
        # Description = ""
      } else{
        
        img  = paste0("<img src='" , img_url,  "' width = 100% />")
        HTML(img)
      }
      
      }
    
    
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
