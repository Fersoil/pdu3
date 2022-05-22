library(shiny)
library(leaflet)
library(data.table)
library(RColorBrewer)
library(lubridate)


update_data = F
date_interval <- 10
  
# source("helpers.R")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

color_pallete <- "RdYlBu"


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
      checkboxInput("legend", "Show legend", TRUE)
    ),
    mainPanel(
      leafletOutput("mymap"),
      leafletOutput("heatMap")
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
  
  colorpal <- reactive({
     colorNumeric(color_pallete, filteredData()$n)
  })
  
  
  output$mymap <- renderLeaflet({
    st = stations()
    leaflet(st) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))

  })


  observe({
    pal <- colorpal()
    
    leafletProxy("mymap", data = filteredData()) %>%
      clearMarkers() %>%
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
  })
  
}

shinyApp(ui, server)
