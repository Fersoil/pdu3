library(shiny)
library(leaflet)
library(data.table)
library(RColorBrewer)


update_data = F
if(update_data)
  source("helpers.R")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()



ui <- fluidPage(
  titlePanel("Citi Bikes in New York"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("in_out",
                  label = "Choose departure or arrivals",
                  choices = c("in", "out"),
                  selected = "in"),
      sliderInput("range", "Number of rents", min(in_stations$n), max(in_stations$n),
                  value = range(in_stations$n), step = 1),
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
  stations_data <- reactive({
    stations_data <- switch(input$in_out,
                 "in" = in_stations,
                 "out" = out_stations)
  })
  
  filteredData <- reactive({
    st = stations_data()
    st[st$n >= input$range[1] & st$n <= input$range[2],]
    st
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, stations_data()$n)
  })
  
  
  
  output$mymap <- renderLeaflet({
    st = stations_data()
    leaflet(st) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))

  })
  
  


  observe({
    pal <- colorpal()
    data = filteredData()
    
    leafletProxy("mymap", data = data) %>%
      clearShapes() %>%
      addCircles(lng = ~lng, lat = ~lat,
                 weight = 1,
                       fillColor = ~pal(n),
                       color = "#777777",
                       popup = ~paste(n),
                       stroke = FALSE,
                       radius = ~log(n)*100,
                       fillOpacity = 0.8)
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