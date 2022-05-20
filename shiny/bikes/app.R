library(shiny)
library(leaflet)
library(data.table)
library(RColorBrewer)
library(lubridate)


update_data = F
if(update_data)
  source("helpers.R")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

colpal <- "stataat"

ui <- fluidPage(
  titlePanel("Citi Bikes in New York"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("in_out",
                  label = "Choose departure or arrivals",
                  choices = c("in", "out"),
                  selected = "in"),
      sliderInput("range", "Number of rents", min(in_stations$n), max(in_stations$n), #TODO
                  value = range(in_stations$n), step = 1),
      dateRangeInput("date",
                     label="choose beggining and ending of a span",
                     format = "yyyy-mm-dd"),
      sliderInput(
        "test_input",
        label = "Select time",
        min = lubridate::origin,
        max = lubridate::origin + days(1) - seconds(1),
        value = c(lubridate::origin, lubridate::origin + days(1) - seconds(1)),
        step = 5 * 60,
        timeFormat = "%H:%M",
        timezone = "+0000",
        ticks = FALSE
      ),
      selectInput("colors", "Color Scheme",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      ),
      checkboxInput("legend", "Show legend", TRUE)
    ),
    mainPanel(
      leafletOutput("mymap")
    )
    
  )
)

server <- function(input, output, session) {
  stations <- reactive({
    stations <- switch(input$in_out,
                 "in" = in_stations,
                 "out" = out_stations)
  })
  
  filteredData <- reactive({
    st = stations()
    st[n >= input$range[1] & n <= input$range[2], ,]
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, stations()$n)
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
                       fillOpacity = 0.9)
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = stations())

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