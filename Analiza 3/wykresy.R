## here is some code to provide plots for some choosen neighborhoods
## We weill focus on 4 neighborhoods
# east village, gramersy, hell's kitchen, chelsey, midtown, finantual district, dumbo, williamsburg
# 

## kod powinien byc wykonywany po uruchomieniu aplikacji shiny, w celu poprawnego zaimportowaniu danych

borough_list = c("East Village", "Upper West Side", "Financial District", "Midtown", "Central Park", "Williamsburg", "Gramercy", "Chelsea")
library(ggplot2)
library(plotly)
# dane grupujemy w okresy 5 minutowe

prepare_to_plot <- function(df) {
  # funkcja przygotowuje dane do wykresów - to samo co w shiny
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
                  ][weekday!="sobota" & weekday!="niedziela",  # chcemy tylko dni powszednie
                  ][, .(time = (time %/% 300) *300, neighborhood) # zaokrąglenie czasu - okresy 5 min
                  ][, .(rides = .N), by=.(time, neighborhood)]
}


# chcemy dwa wykresy
in_to_plot <- prepare_to_plot(in_stations)
out_to_plot <- prepare_to_plot(out_stations)


### rozwiazanie niekonieczne poprawne

plot_districts <- function(df, title, showlegend=F) {
  df <- df %>%
    group_by(neighborhood)
  
  plt <- plot_ly(df, x=~time, y=~rides, color = ~neighborhood,
                 mode = "lines", type = "scatter", 
                 legendgroup=~neighborhood, showlegend = showlegend) %>%
    layout(xaxis = list(title = title))
}


plt1 <- plot_districts(in_to_plot, "przyjazdy", showlegend=T)
plt2 <- plot_districts(out_to_plot, "odjazdy")

fig <- subplot(plt1, plt2, nrows = 1, 
               titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
  layout(title = "Liczba odjazdów oraz przyjazdów do dzielnic",
         zerolinewidth = 2)

## koniec rozwiazania sredniopoprawnego



# robimy wykresy
plot1 <- ggplot(in_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "odjazdy z dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))


fig1 <- ggplotly(plot1, showlegend=TRUE, grouplegend=~neighborhood) %>%
  layout(xaxis = list(title="liczba przyjazdów do dzielnicy"))

plot2 <- ggplot(out_to_plot, aes(time, rides, group=neighborhood)) +
  geom_line(aes(colour = neighborhood), size=1.2) +
  xlab("godzina") +
  labs(title = "Liczba wypożyczeń rowerów w danych dzielnicach", subtitle = "przyjazdy do dzielnicy") +
  scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"))

fig2 <- ggplotly(plot2, showlegend=FALSE, grouplegend=~neighborhood) %>%
  layout(xaxis = list(title="liczba wyjazdów z dzielnicy"))

fig <- subplot(fig1, fig2, nrows = 1, 
               titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
  layout(title = "Liczba odjazdów oraz przyjazdów do dzielnic",
         zerolinewidth = 2)


fig

