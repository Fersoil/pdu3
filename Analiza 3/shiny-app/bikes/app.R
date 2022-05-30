library(leaflet)
library(dplyr)
library(stringi)
library(data.table)
library(shiny)
library(leaflet.extras)
library(RColorBrewer)
library(lubridate)
library(sf)
library(httr)
library(maptools)
library(rgdal)

getwd()

update_data = F
date_interval <- 10
  
# source("helpers.R")

#tidygeocoder

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

color_pallete <- "RdYlBu"
tile_color_pallete <- "YlOrRd"


ui <- fluidPage(
  titlePanel("Citi Bikes in New York"),
  sidebarLayout(
    sidebarPanel(
      selectInput("in_out",
                  label = "Choose departure or arrivals",
                  choices = c("in", "out"),
                  selected = "out"),
      sliderInput("range", "Number of rents", min(pre_stat$n), max(pre_stat$n), #TODO
                  value = range(pre_stat$n), step = 1),
      dateRangeInput("date",
                     label="choose beggining and ending of a span",
                     format = "yyyy-mm-dd", start="2019-01-01", end="2019-12-31"),
      sliderInput(
        "time_input",
        label = "Select time",
        min = lubridate::origin,
        max = lubridate::origin + days(1) - seconds(1),
        value = c(lubridate::origin, lubridate::origin + days(1) - seconds(1)),
        step = 5 * 60,
        timeFormat = "%H:%M",
        timezone = "+0000",
        ticks = FALSE
      ),
      checkboxInput("legend", "Show legend", FALSE),
      selectInput("maptype",
                  label = "Choose map type",
                  choices = c("markers", "heatmap", "tiles"),
                  selected = "markers")
    ),
    mainPanel(
      leafletOutput("mymap",
                    width = "100%", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  ## functions below create a pipeline of filtering data - passing through all posiible inputs
  ## starting from the one requring most computations
  
  stations <- reactive({ # function handles in out input - deciding whether we want to show arrivals or departures from stations
    stations <- switch(input$in_out,
                 "in" = in_stations,
                 "out" = out_stations)
  })
  
  datedData <- reactive({ # function handles data filtering 
     print(input$date)
     date_interval <- interval(input$date[1], input$date[2])
     st = stations()
     st[date %within% date_interval, ] # returns information about rents in within a specified range of time
  })
  
  timedData <- reactive({ # function handles time filtering
    st = datedData() # 
    st[between(time, as.ITime(input$time_input[1]), as.ITime(input$time_input[2]))
               ][, .(n = .N), by=.(lat, lng, name)]
  })
  
  filteredData <- reactive({ # filtering number of rents - it allows us to filter some stations
    st = timedData()
    st[n >= input$range[1] & n <= input$range[2], ,]
  })
  
  points <- reactive({ # in order to create chloroplet map, we need to prepare stations as sf class object
    st <- filteredData()
    coords <- st[, .(lng, lat)]
    points <- SpatialPointsDataFrame(coords, data = st)
    
    
    points <- st_as_sf(points)
    points <- points %>% st_set_crs(st_crs(nyc_polygons))
  })
  
  summarized_polygons <- reactive({ # function joins stations with polygons that describe nyc neighborhoods
    # it uses previously prepared points and polygons - all of sf class
    points <- points()
    joined_polygons <- st_join(nyc_polygons, points, left=FALSE)
    joined_polygons <- as.data.table(joined_polygons)
    joined_polygons <- joined_polygons[, .(neighborhood, boroughCode, borough, n, geometry)]
    
    joined_polygons <- joined_polygons[, .(sum=sum(n), num_of_stat=.N), by=.(neighborhood, boroughCode, borough)]
    joined_polygons <- merge(joined_polygons, nyc_polygons)
    
    polygons <- st_as_sf(joined_polygons)
    polygons$area <- st_area(polygons)
    polygons$avg <- polygons$sum / polygons$area # calculating average rents

    
    polygons
  })
  
  polygon_labels <- reactive({ # functions adds labels to polygon dataframe
    polygons <- summarized_polygons()
    labels <- sprintf(
      "<strong>%s</strong><br/>%g rents",
      polygons$neighborhood, polygons$sum
    ) %>% lapply(htmltools::HTML)
    
    labels
  })
  
  tilecolorpal <- reactive({
    pal <- colorBin(tile_color_pallete, domain = summarized_polygons()$sum, bins = 5)
  })
  
  colorpal <- reactive({
     colorNumeric(color_pallete, filteredData()$n)
  })
  
  output$mymap <- renderLeaflet({ # preparing leaflet map to reactive operations
    st = stations()
    leaflet(st) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    pal <- colorpal() #choosing palettes
    tile_pal <- tilecolorpal() 
    
    map <- leafletProxy("mymap", data = filteredData()) %>% # cleaning the map
      clearMarkers() %>% 
      clearHeatmap() %>%
      clearShapes()

    if(input$maptype == "markers"){
      map <- map %>%
        addCircleMarkers(lng = ~lng, lat = ~lat,
                         weight = 1,
                         fillColor = ~pal(n),
                         color = "#777777",
                         popup = ~paste(n),
                         stroke = FALSE,
                         radius = 5,
                         label = ~name,
                         fillOpacity = 0.9
        )
    }
    else if(input$maptype == "heatmap"){
      map <- map %>%
        addHeatmap(lng=~lng, lat=~lat, gradient=color_pallete,intensity=~n, 
                   blur = 15, max = ~max(n), radius = 10)
    }
    else {
      polygon <- summarized_polygons()
      labels <- polygon_labels()
      
      map <- leafletProxy("mymap", data = polygon) %>% # cleaning the map again
        clearMarkers() %>% 
        clearHeatmap() %>%
        clearShapes() %>%
        addPolygons(fillColor = ~tile_pal(sum),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label = labels,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        setView(lat = new_york_cords[2], lng = new_york_cords[1], zoom=12)
    }
    
    return(map)
  })
  
  observe({
    
    
    if(input$maptype == "tiles") {
      proxy <- leafletProxy("mymap", data=summarized_polygons())
      proxy %>% clearControls()
      tile_pal <- tilecolorpal()
      
      proxy %>% addLegend(position = "bottomright",
                          pal = tile_pal, values = ~sum,
                          opacity = 0.7, title = "number of rents"
      )
    }
    else {
      proxy <- leafletProxy("mymap", data=filteredData())
      proxy %>% clearControls()
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~n,
                          title = "number of rents"
      )
    }
    
  })
  
}

shinyApp(ui, server)
