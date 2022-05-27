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
      # , zakomentowane wybieranie koloru
      # selectInput("colors", "Color Scheme",
      #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      #),
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
  stations <- reactive({
    stations <- switch(input$in_out,
                 "in" = in_stations,
                 "out" = out_stations)
  })
  
  allDatedStations <- reactive({
    datedData()[, .(n = .N), by=.(lat, lng, name)]
  })
  
  datedData <- reactive({
     print(input$date)
     date_interval <- interval(input$date[1], input$date[2])
     
     print(date_interval)
     st = stations()
     print(head(st$date))
     st[date %within% date_interval, ]
  })
  
  timedData <- reactive({
    st = datedData()
    st[between(time, as.ITime(input$time_input[1]), as.ITime(input$time_input[2]))
               ][, .(n = .N), by=.(lat, lng, name)]
  })
  
  filteredData <- reactive({
    st = timedData()
    timedData()
    st[n >= input$range[1] & n <= input$range[2], ,]
  })
  
  points <- reactive({
    st <- filteredData()
    coords <- st[, .(lng, lat)]
    points <- SpatialPointsDataFrame(coords, data = st)
    
    
    points <- st_as_sf(points)
    points <- points %>% st_set_crs(st_crs(nyc_polygons))
  })
  
  summarized_polygons <- reactive({ # function joins stations with polygons that describe nyc neighborhoods
    points <- points()
    joined_polygons <- st_join(nyc_polygons, points, left=FALSE)
    joined_polygons <- as.data.table(joined_polygons)
    joined_polygons <- joined_polygons[, .(neighborhood, boroughCode, borough, n, geometry)]
    
    joined_polygons <- joined_polygons[, .(sum=sum(n)), by=.(neighborhood, boroughCode, borough)]
    joined_polygons <- merge(joined_polygons, nyc_polygons)
    
    polygons <- st_as_sf(joined_polygons)
    polygons$area <- st_area(polygons)
    polygons$avg <- polygons$sum / polygons$area

    
    polygons
  })
  
  polygon_labels <- reactive({ # functions adds labels to polygon dataframe
    polygons <- summarized_polygons()
    labels <- sprintf(
      "<strong>%s</strong><br/>%g rents / m<sup>2</sup>",
      polygons$neighborhood, polygons$avg
    ) %>% lapply(htmltools::HTML)
    
    labels
  })
  
  tilecolorpal <- reactive({
    pal <- colorBin(tile_color_pallete, domain = summarized_polygons()$avg, bins = 5)
  })
  
  colorpal <- reactive({
     colorNumeric(color_pallete, filteredData()$n)
  })
  
  output$mymap <- renderLeaflet({
    st = stations()
    leaflet(st) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    pal <- colorpal()
    tile_pal <- tilecolorpal()
    
    map <- leafletProxy("mymap", data = filteredData()) %>%
      clearMarkers()

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
      
      map <- leafletProxy("mymap", data = polygon) %>%
        clearMarkers() %>%
        addPolygons(fillColor = ~tile_pal(avg),
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
        addProviderTiles("CartoDB.Positron") 
    }
    
    return(map)
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = filteredData())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~n
      )
    }
    if(input$maptype == "tiles") {
      proxy <- leafletProxy("mymap", data=summarized_polygons())
      proxy %>% clearControls()
      tile_pal <- tilecolorpal()
      
      proxy %>% addLegend(position = "bottomright",
                          pal = tile_pal, values = ~avg,
                          opacity = 0.7, title = "rents per square meter"
      )
    }
  })
  
}

shinyApp(ui, server)
