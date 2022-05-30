## here is some code to provide plots for some choosen neighborhoods
## We weill focus on 4 neighborhoods
# east village, gramersy, hell's kitchen, chelsey, midtown, finantual district, dumbo, williamsburg
# 

borough_list = c("East Village", "Upper West Side", "Financial District", "Midtown", "Central Park", "Williamsburg")
library(ggplot2)
library(plotly)
# dane grupujemy w okresy 5 minutowe

prepare_to_plot <- function(df) {
  coords <- df[, .(lng, lat)]
  points <- SpatialPointsDataFrame(coords, data = df)
  
  
  points <- st_as_sf(points)
  points <- points %>% st_set_crs(st_crs(nyc_polygons))
  
  poly <- st_join(nyc_polygons, points, left=FALSE)
  poly <- st_drop_geometry(poly)
  poly <- poly[c("neighborhood", "date", "time")]
  
  poly <- as.data.table(poly)
  
  
  to_plot <- poly[neighborhood %in% borough_list, 
                  ][, .(weekday=weekdays(date), neighborhood, time)
                  ][weekday!="sobota" & weekday!="niedziela", 
                  ][, .(time = (time %/% 300) *300, neighborhood)
                  ][, .(rides = .N), by=.(time, neighborhood)]
}

in_to_plot <- prepare_to_plot(in_stations)
out_to_plot <- prepare_to_plot(out_stations)


test <- in_stations[, .(time= (time %/% 300) * 300 )
                    ][, .(rides = .N), by=.(time)]
head(test)

plot1 <- ggplot(in_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "odjazdy z dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))


fig1 <- ggplotly(plot1, showlegend=TRUE) %>%
  layout(xaxis = list(title="liczba przyjazdów do dzielnicy"))

plot2 <- ggplot(out_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "przyjazdy do dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))

fig2 <- ggplotly(plot2, showlegend=FALSE) %>%
  layout(xaxis = list(title="liczba wyjazdów z dzielnicy"))

fig <- subplot(fig1, fig2, nrows = 1, 
               titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
  layout(title = "Liczba odjazdów oraz przyjazdów do dzielnic",
         zerolinewidth = 2)


fig
