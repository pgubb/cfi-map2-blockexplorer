library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(tidyverse)
library(waiter)

source("utils.R")

# Define a dictionary mapping city names to their respective countries
city_country_mapping <- c(
  "Lagos" = "Nigeria",
  "São Paulo" = "Brazil",
  "Jakarta" = "Indonesia",
  "Addis Ababa" = "Ethiopia",
  "Delhi" = "India"
)

block_category_labels <- c(
  `1` = "Off-limits", 
  `2` = "Empty/not-urban", 
  `3` = "Insecure", 
  `4` = "Strata 1: High probability", 
  `5` = "Strata 2: Low probability"
)

# Define UI

ui <- page_sidebar(

  title = "CFI/MAP2 Block Explorer",
  sidebar = sidebar(
    style = "font-size:12px", 
    "Explore and visualize the geospatial sampling grid and initial sampled blocks supprting adaptive cluster sampling for the CFI/MAP2 demand-side study.",
    selectInput("city", "Choose a city:", choices = names(city_country_mapping), selected = "São Paulo"),
    selectInput("polygon_id", "Select Block ID (of final sample of ~100 blocks):", choices = NULL, selected = ""), 
    code(style = "font-size:12px", icon("location-dot"), "Centroid of selected block (link opens in Google Maps):", uiOutput("coordinates_url", inline = TRUE)), 
    sliderInput(
      "expansions_N",
      "Show the adjacent blocks after 'N' outward expansions:",
      min = 0,
      max = 7,
      value = 0
    ),
    hr(),
    span(style = "color:grey; font-size:10px", 
      "Notes:", 
      br(), br(), 
      "1. The methodology for defining the sampling boundary and grid is explained in detail", 
      a("here", href='https://github.com/pgubb/cfi-map2-sampling-grids/tree/main/documentation/'),
      br(), br(), 
      "2. The ESRI World Imagery base map provides one meter or better satellite and aerial imagery in many parts of the world and lower resolution satellite imagery worldwide, images are typically captured within the last 3-5 years.")
  ),
    card(
      leafletOutput("map", height = 1000)
    )
)


# Server logic
server <- function(input, output, session) {
  
  grid <- reactive({
    
    country <- city_country_mapping[[input$city]]
    
    # Loading geojson grid
    geojson_path <- paste0("data/", country, "/", "final_sampling_grid", ".geojson")
    grid <- st_read(geojson_path, quiet = TRUE)
    
    # Importing partner categorization of blocks 
    file_path <- paste0("data/", country, "/", "stage1_blocks_final", ".csv")
    
    if(file.exists(file_path)) {
      
      stage1_blocks <- read_csv(file_path) %>% 
        mutate(
               block_id = as.character(block_id), 
               )
      
      # Merging selections into grid
      grid %>% left_join(stage1_blocks, by = c("block_id")) -> grid
    
    } else { 
      
      grid %>% mutate(in_final_sample = ifelse(block_category_str %in% c("Strata 1: High probability", "Strata 2: Low probability"), 1, 0)) -> grid
      
      }
    
    # Computing a string with the centroids of all the blocks in the grid
    centroids <- st_centroid(grid) %>% 
      select(block_id, geometry) %>% 
      mutate(
            lon = st_coordinates(.)[,1],  
            lat = st_coordinates(.)[,2], 
             centroid_str = paste(lat,lon, sep = ", "), 
             url = paste("<a ", paste0("href=", sprintf("https://www.google.com/maps/search/?api=1&query=%.16f,%.16f", lat, lon)), ">", centroid_str, "</a>"), 
             block_label2 = paste(sep = "<br/>", "[Block ID:] ", block_id, "[Centroid coordinates:] ", url) 
             ) %>% 
      select(block_id, centroid_str, block_label2) %>% st_drop_geometry() %>% as.data.frame() 
    
    grid <- grid %>% left_join(centroids, by = "block_id")
    
    return(grid)
    
  })
  
  center_coords_map <- reactive({
    bbox <- st_bbox(grid())
    coords_x <- (bbox[[1]] + bbox[[3]])/2 
    coords_y <-  (bbox[[2]] + bbox[[4]])/2
    return(c(coords_x, coords_y))
  })
  
  # Observe changes in the selected city to re-compute block ids
  observeEvent(grid(), {
    
    # Extract block_ids of blocks in the intial sample: 
    block_ids <- unique(grid()[grid()$in_final_sample == 1, ]$block_id)
    block_ids <- block_ids[!is.na(block_ids)]
    # Update selectInput for polygon_id with block_ids
    updateSelectInput(session, "polygon_id", choices = block_ids)
    
  })
  
  
  # Blocks in initial sample (N = 125)
  sampled_polys <- reactive({
    grid()[grid()$in_sample == 1, ]
  })
  
  #. BLocks in final sample (N = 100)
  selected_polys <- reactive({
    grid()[grid()$in_final_sample == 1, ]
  })
  
  
  # Selecting the "origin" polygon
  origin_poly <- reactive({
    grid()[grid()$block_id == input$polygon_id, ]
  })
  
  origin_center <- reactive({ 
    st_centroid(origin_poly())
  })
  
  # Finding the polygons that are adjacent to origin
  adjacent_polys <- reactive({
    if (input$polygon_id != "" & input$expansions_N > 0){
      expand_network(input$polygon_id, grid(), input$expansions_N)
    }
  })
  
  ##########################
  # OUTPUTS ---------------
  ##########################
  
  output$coordinates_url <- renderUI({

      lon <- st_coordinates(origin_center())[, 1]
      lat <- st_coordinates(origin_center())[, 2]
      url <- a(paste(lat, lon, sep = ", "), href = sprintf("https://www.google.com/maps/search/?api=1&query=%.16f,%.16f", lat, lon))
      HTML(paste(url))

  })
  
  # BASEMAP ------------ 
  output$map <- renderLeaflet({
    
    # Aspects of map that won't change dynamically
    leaflet() %>%
      #addTiles(group = "OSM (Default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      addProviderTiles(providers$Stadia.AlidadeSmooth, group = "Stadia Grey (for contrast)") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM", "ESRI World Imagery", "Stadia Grey (for contrast)"),
        overlayGroups = c("Sampling grid", "Initial sample (N = 125 blks)", "Final sample (N ~ 100 blks)", "Adjacent blocks"), 
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      setView(center_coords_map()[1], center_coords_map()[2], zoom = 12) 
    
  })
  
  # COUNTRY-SPECIFIC OVERLAYS: SAMPLING GRIDS ------------ 
  observe({
    
    pal <- colorFactor(
      palette = c('#E4FF1A', '#FFB800', '#FF5714', 'blue', 'purple'),
      domain = sampled_polys()$block_category_str
    )
  
    # Aspects of map that will change dynamically
    
    leafletProxy("map") %>%
      clearControls() %>%
      addPolygons(data = grid(), weight = 1, color = "black", fill = FALSE, group = "Sampling grid") %>% 
      addPolygons(data = sampled_polys(), label = ~block_label, popup = ~block_label2, weight = 2, color =  ~pal(block_category_str), fill =TRUE, fillOpacity = 0.3, group = "Initial sample (N = 125 blks)") %>% 
      addPolygons(data = selected_polys(), label = ~block_id, weight = 4, color = "green", fill = FALSE, group = "Final sample (N ~ 100 blks)") %>% 
      addLegend("bottomright",
                pal = pal, 
                values = sampled_polys()$block_category_str,
                title = "Block categories",
                opacity = 1, 
                group = "Initial sample (N = 125 blks)"
      ) %>%
      flyTo(
        lng = st_centroid(st_union(grid()))[[1]][1], 
        lat = st_centroid(st_union(grid()))[[1]][2], 
        zoom = 12
      )
    
  })
  
  # COUNTRY-SPECIFIC OVERLAYS: SELECTED BLOCKS ------------ 
  observe({
    
    # Aspects of map that change when the selected polygon_id is changed
    
    if (input$polygon_id != "") {
    
    poly_center_x <- origin_center()$geometry[[1]][1]
    poly_center_y <- origin_center()$geometry[[1]][2]
    
    leafletProxy("map") %>%
      clearGroup("Selected block") %>%
      clearMarkers() %>%
      addPolygons(data = origin_poly(), weight = 2, color = "blue", fill = FALSE, group = "Selected block") %>%
      addLabelOnlyMarkers(data =  origin_poly(),
                            lng = poly_center_x, 
                            lat = poly_center_y, 
                            label = ~block_id, 
                            layerId = "block_id_mrkr", 
                            labelOptions = labelOptions(noHide = TRUE, 
                                                        direction = 'center', 
                                                        textOnly = TRUE, 
                                                        textsize = "15px"
                                                       )) %>% 
   
      flyTo(
        lng = poly_center_x,
         lat = poly_center_y,
         zoom = 16
      ) 
      
    }
    
    # COUNTRY-SPECIFIC OVERLAYS: ADJACENT BLOCKS ------------ 
    
    observe({
      
      # Aspects of map that change when the selected polygon_id is changed
      
      if (input$polygon_id != "" & input$expansions_N > 0) {
        
        leafletProxy("map") %>% clearGroup("Adjacent blocks") %>%
          addPolygons(data = adjacent_polys(), label = ~block_id, popup = ~block_label2, weight = 1, color = "orange", fill = TRUE, fillOpacity = 0.3, group = "Adjacent blocks")  

      }
      
    })
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
